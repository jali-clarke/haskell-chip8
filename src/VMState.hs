{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module VMState
  ( VMState,
    Action,
    withNewVMState,
    runAction,
    withMemoryAction,
    withRegistersAction,
    withScreenBufferAction,
    withStackAction,
    withTimersAction,
    getOpCodeBin,
    incrementPC,
    setPC,
  )
where

import BaseTypes
import qualified Control.Monad.Except as MTL
import qualified Control.Monad.State.Strict as MTL
import Data.Bits (unsafeShiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.Finite as Finite
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import qualified GHC.TypeLits.Compare as TypeNats
import qualified GHC.TypeNats as TypeNats
import qualified SizedByteString
import TypeNatsHelpers
import VMState.Memory (Memory)
import qualified VMState.Memory as Memory
import VMState.Registers (Registers)
import qualified VMState.Registers as Registers
import VMState.ScreenBuffer (ScreenBuffer)
import qualified VMState.ScreenBuffer as ScreenBuffer
import VMState.Stack (Stack)
import qualified VMState.Stack as Stack
import VMState.Timers (Timers)
import qualified VMState.Timers as Timers

type ProgramCounter = MemoryAddress

data VMState stackSize = VMState
  { memory :: Memory,
    registers :: Registers,
    screenBuffer :: ScreenBuffer,
    stack :: Stack stackSize,
    timers :: Timers,
    pc :: MemoryAddress
  }

newtype Action stackSize a = Action (MTL.ExceptT String (MTL.StateT (VMState stackSize) IO) a) deriving (Functor, Applicative, Monad)

withNewVMState :: Int -> ByteString -> (forall stackSize. Either String (VMState stackSize) -> IO r) -> IO r
withNewVMState maxStackSize programRom callback =
  SizedByteString.withSized programRom $ \sizedProgramRom -> do
    let byteStringSize = SizedByteString.length' sizedProgramRom
    case TypeNats.sameNat byteStringSize (Proxy :: Proxy 0) of
      Just Refl -> callback (Left "program is empty")
      Nothing ->
        case TypeNats.isLE byteStringSize (Proxy :: Proxy MemorySize) of
          Nothing -> callback (Left "program rom is too large")
          Just Refl -> do
            loadedMemory <- Memory.memoryWithLoadedProgram sizedProgramRom
            Stack.withNewStack maxStackSize $ \newStack -> do
              newRegisters <- Registers.newRegisters
              newScreenBuffer <- ScreenBuffer.newScreenBuffer
              let newState =
                    VMState
                      { memory = loadedMemory,
                        registers = newRegisters,
                        screenBuffer = newScreenBuffer,
                        stack = newStack,
                        timers = Timers.newTimers,
                        pc = Finite.finite 0
                      }
              callback (Right newState)

runAction :: Action stackSize a -> VMState stackSize -> IO (Either String a, VMState stackSize)
runAction (Action action) vmState = MTL.runStateT (MTL.runExceptT action) vmState

withMemoryAction :: Memory.Action a -> Action stackSize a
withMemoryAction memoryAction =
  Action $ do
    thisMemory <- MTL.gets memory
    MTL.liftIO $ Memory.runAction memoryAction thisMemory

withRegistersAction :: Registers.Action a -> Action stackSize a
withRegistersAction registersAction =
  Action $ do
    vmState <- MTL.get
    (result, newRegisters) <- MTL.liftIO $ Registers.runAction registersAction (registers vmState)
    MTL.put (vmState {registers = newRegisters})
    pure result

withScreenBufferAction :: ScreenBuffer.Action a -> Action stackSize a
withScreenBufferAction screenBufferAction =
  Action $ do
    thisScreenBuffer <- MTL.gets screenBuffer
    MTL.liftIO $ ScreenBuffer.runAction screenBufferAction thisScreenBuffer

withStackAction :: Stack.Action stackSize a -> Action stackSize a
withStackAction stackAction =
  Action $ do
    vmState <- MTL.get
    (maybeResult, newStack) <- MTL.liftIO $ Stack.runAction stackAction (stack vmState)
    MTL.put (vmState {stack = newStack})
    case maybeResult of
      Left err -> MTL.throwError err
      Right result -> pure result

withTimersAction :: Timers.Action a -> Action stackSize a
withTimersAction timersAction =
  Action $ do
    vmState <- MTL.get
    let (result, newTimers) = Timers.runAction timersAction (timers vmState)
    MTL.put (vmState {timers = newTimers})
    pure result

getOpCodeBin :: Action stackSize OpCodeBin
getOpCodeBin = do
  vmState <- Action MTL.get
  let currentPC = pc vmState
  case addOne currentPC of
    Nothing -> Action $ MTL.throwError "program counter (pc) is misaligned; pc + 1 is out of the address range"
    Just currentPCPlusOne ->
      withMemoryAction $ do
        -- opcodes stored big-endian
        op0 <- fmap fromIntegral $ Memory.readMemory currentPC
        op1 <- fmap fromIntegral $ Memory.readMemory currentPCPlusOne
        pure $ unsafeShiftL op0 8 .|. op1

incrementPC :: Action stackSize ()
incrementPC = do
  currentPC <- Action $ MTL.gets pc
  case addTwo currentPC of
    Nothing -> Action $ MTL.throwError "incremented past end of vm memory"
    Just nextPC -> setPC nextPC

setPC :: ProgramCounter -> Action stackSize ()
setPC nextPC = Action $ MTL.modify (\vmState -> vmState {pc = nextPC})
