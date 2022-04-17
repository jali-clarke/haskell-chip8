{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module VM
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
    getPC,
    setPC,
    randomByte,
    getKeyboardKey,
    throwVMError,
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
import Data.Word (Word8)
import qualified GHC.TypeLits.Compare as TypeNats
import qualified GHC.TypeNats as TypeNats
import qualified SizedByteString
import TypeNatsHelpers
import VM.MachineCallbacks (MachineCallbacks)
import qualified VM.MachineCallbacks as MachineCallbacks
import VM.Memory (Memory)
import qualified VM.Memory as Memory
import VM.Registers (Registers)
import qualified VM.Registers as Registers
import VM.ScreenBuffer (ScreenBuffer)
import qualified VM.ScreenBuffer as ScreenBuffer
import VM.Stack (Stack)
import qualified VM.Stack as Stack
import VM.Timers (Timers)
import qualified VM.Timers as Timers

data VMState stackSize = VMState
  { memory :: Memory,
    registers :: Registers,
    screenBuffer :: ScreenBuffer,
    stack :: Stack stackSize,
    timers :: Timers,
    pc :: MemoryAddress,
    machineCallbacks :: MachineCallbacks
  }

newtype Action stackSize a = Action (MTL.ExceptT String (MTL.StateT (VMState stackSize) IO) a) deriving (Functor, Applicative, Monad)

withNewVMState :: MachineCallbacks -> Int -> ByteString -> (forall stackSize. Either String (VMState stackSize) -> IO r) -> IO r
withNewVMState theseMachineCallbacks maxStackSize programRom callback =
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
                      { machineCallbacks = theseMachineCallbacks,
                        memory = loadedMemory,
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

getPC :: Action stackSize MemoryAddress
getPC = Action $ MTL.gets pc

setPC :: MemoryAddress -> Action stackSize ()
setPC nextPC = Action $ MTL.modify (\vmState -> vmState {pc = nextPC})

randomByte :: Action stackSize Word8
randomByte = withMachineCallbacks MachineCallbacks.randomByte

getKeyboardKey :: Action stackSize KeyboardKey
getKeyboardKey = do
  keyChar <- withMachineCallbacks MachineCallbacks.blockingGetKeyboardKey
  case keyChar of
    '0' -> pure $ Finite.finite 0
    '1' -> pure $ Finite.finite 1
    '2' -> pure $ Finite.finite 2
    '3' -> pure $ Finite.finite 3
    '4' -> pure $ Finite.finite 4
    '5' -> pure $ Finite.finite 5
    '6' -> pure $ Finite.finite 6
    '7' -> pure $ Finite.finite 7
    '8' -> pure $ Finite.finite 8
    '9' -> pure $ Finite.finite 9
    'a' -> pure $ Finite.finite 10
    'b' -> pure $ Finite.finite 11
    'c' -> pure $ Finite.finite 12
    'd' -> pure $ Finite.finite 13
    'e' -> pure $ Finite.finite 14
    'f' -> pure $ Finite.finite 15
    _ -> getKeyboardKey

throwVMError :: String -> Action stackSize a
throwVMError errMsg = Action $ MTL.throwError errMsg

withMachineCallbacks :: (MachineCallbacks -> IO a) -> Action stackSize a
withMachineCallbacks callbackCallback =
  Action $ do
    theseMachineCallbacks <- MTL.gets machineCallbacks
    MTL.liftIO $ callbackCallback theseMachineCallbacks
