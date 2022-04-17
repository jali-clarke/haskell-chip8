{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module VMState
  ( VMState,
    VMExec,
    withNewVMState,
    runVM,
    withMemoryAction,
    getsRegisters,
    withRegisters,
    modifyRegisters,
    withStackAction,
    getsTimers,
    modifyTimers,
    getOpCodeBin,
    incrementPC,
    setPC,
  )
where

import BaseTypes
import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as MTL
import Control.Monad.State.Strict (StateT)
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
import VMState.Stack (Stack)
import qualified VMState.Stack as Stack
import VMState.Timers (Timers)
import qualified VMState.Timers as Timers

type ProgramCounter = MemoryAddress

data VMState stackSize = VMState {memory :: Memory, stack :: Stack stackSize, registers :: Registers, timers :: Timers, pc :: MemoryAddress}

newtype VMExec stackSize a = VMExec (ExceptT String (StateT (VMState stackSize) IO) a) deriving (Functor, Applicative, Monad)

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
              let newState =
                    VMState
                      { memory = loadedMemory,
                        stack = newStack,
                        registers = newRegisters,
                        timers = Timers.newTimers,
                        pc = Finite.finite 0
                      }
              callback (Right newState)

runVM :: VMState stackSize -> VMExec stackSize a -> IO (Either String a, VMState stackSize)
runVM vmState (VMExec action) = MTL.runStateT (MTL.runExceptT action) vmState

getsTimers :: (Timers -> a) -> VMExec stackSize a
getsTimers projectTimers = VMExec $ MTL.gets (projectTimers . timers)

modifyTimers :: (Timers -> Timers) -> VMExec stackSize ()
modifyTimers timersUpdate = VMExec $ MTL.modify (\vmState -> vmState {timers = timersUpdate (timers vmState)})

withMemoryAction :: (forall m. MTL.MonadIO m => Memory -> m a) -> VMExec stackSize a
withMemoryAction memoryAction = VMExec $ MTL.gets memory >>= memoryAction

getsRegisters :: (Registers -> a) -> VMExec stackSize a
getsRegisters projectRegisters = VMExec $ MTL.gets (projectRegisters . registers)

withRegisters :: (Registers -> IO a) -> VMExec stackSize a
withRegisters registersAction = VMExec $ MTL.gets registers >>= MTL.liftIO . registersAction

modifyRegisters :: (Registers -> Registers) -> VMExec stackSize ()
modifyRegisters registersUpdate = VMExec $ MTL.modify (\vmState -> vmState {registers = registersUpdate (registers vmState)})

withStackAction :: (forall m. (MTL.MonadIO m, MTL.MonadError String m) => Stack stackSize -> m (a, Stack stackSize)) -> VMExec stackSize a
withStackAction stackAction =
  VMExec $ do
    vmState <- MTL.get
    (result, newStack) <- stackAction (stack vmState)
    MTL.put (vmState {stack = newStack})
    pure result

getOpCodeBin :: VMExec stackSize OpCodeBin
getOpCodeBin = do
  vmState <- VMExec MTL.get
  let currentPC = pc vmState
  case addOne currentPC of
    Nothing -> VMExec $ MTL.throwError "program counter (pc) is misaligned; pc + 1 is out of the address range"
    Just currentPCPlusOne ->
      withMemoryAction $ \memoryState -> do
        -- opcodes stored big-endian
        op0 <- fmap fromIntegral $ Memory.readMemory memoryState currentPC
        op1 <- fmap fromIntegral $ Memory.readMemory memoryState currentPCPlusOne
        pure $ unsafeShiftL op0 8 .|. op1

incrementPC :: VMExec stackSize ()
incrementPC = do
  currentPC <- VMExec $ MTL.gets pc
  case addTwo currentPC of
    Nothing -> VMExec $ MTL.throwError "incremented past end of vm memory"
    Just nextPC -> setPC nextPC

setPC :: ProgramCounter -> VMExec stackSize ()
setPC nextPC = VMExec $ MTL.modify (\vmState -> vmState {pc = nextPC})
