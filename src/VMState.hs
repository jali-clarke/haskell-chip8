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
    withMemory,
    getsRegisters,
    withRegisters,
    modifyRegisters,
    popStack,
    pushStack,
    getsTimers,
    modifyTimers,
    getOpCodeBin,
    incrementPC,
    setPC,
  )
where

import BaseTypes
import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Primitive (PrimState)
import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as State
import Data.Bits (unsafeShiftL, (.|.))
import Data.ByteString (ByteString)
import Data.Finite (Finite)
import qualified Data.Finite as Finite
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import qualified Data.Vector.Mutable as BoxedMVector
import qualified Data.Vector.Mutable.Sized as SizedBoxedMVector
import qualified GHC.TypeLits.Compare as TypeNats
import GHC.TypeNats (type (+), type (<=))
import qualified GHC.TypeNats as TypeNats
import qualified SizedByteString
import VMState.Memory (Memory)
import qualified VMState.Memory as Memory
import VMState.Registers (Registers)
import qualified VMState.Registers as Registers
import VMState.Timers (Timers)
import qualified VMState.Timers as Timers

type StackData stackSize = SizedBoxedMVector.MVector stackSize (PrimState IO) MemoryAddress

type ProgramCounter = MemoryAddress

type StackAddress stackSize = Finite stackSize

data Stack stackSize = Stack {stackData :: StackData stackSize, nextStackAddr :: StackAddress stackSize}

data VMState stackSize = VMState {memory :: Memory, stack :: (Stack stackSize), registers :: Registers, timers :: Timers, pc :: MemoryAddress}

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
            unsizedStackData <- BoxedMVector.unsafeNew maxStackSize
            SizedBoxedMVector.withSized unsizedStackData $ \thisStackData -> do
              theseRegisters <- Registers.newRegisters
              let newState =
                    VMState
                      { memory = loadedMemory,
                        stack = Stack {stackData = thisStackData, nextStackAddr = Finite.finite 0},
                        registers = theseRegisters,
                        timers = Timers.newTimers,
                        pc = Finite.finite 0
                      }
              callback (Right newState)

runVM :: VMState stackSize -> VMExec stackSize a -> IO (Either String a, VMState stackSize)
runVM vmState (VMExec action) = State.runStateT (Except.runExceptT action) vmState

popStack :: VMExec stackSize MemoryAddress
popStack =
  VMExec $ do
    stackState <- State.gets stack
    case Finite.sub (nextStackAddr stackState) one of
      Left _ -> Except.throwError "stack underflow"
      Right stackLastElemAddr -> do
        memAddress <- State.liftIO $ SizedBoxedMVector.read (stackData stackState) stackLastElemAddr
        setNextStackAddr stackLastElemAddr
        pure memAddress

pushStack :: (TypeNats.KnownNat stackSize, stackSize <= stackSize + 2) => MemoryAddress -> VMExec stackSize ()
pushStack returnAddr =
  VMExec $ do
    stackState <- State.gets stack
    let newStackLastElemAddr = nextStackAddr stackState
    case addOne newStackLastElemAddr of
      Nothing -> Except.throwError "stack overflow"
      Just newStackNextElemAddr -> do
        State.liftIO $ SizedBoxedMVector.write (stackData stackState) newStackLastElemAddr returnAddr
        setNextStackAddr newStackNextElemAddr

getsTimers :: (Timers -> a) -> VMExec stackSize a
getsTimers projectTimers = VMExec $ State.gets (projectTimers . timers)

modifyTimers :: (Timers -> Timers) -> VMExec stackSize ()
modifyTimers timersUpdate = VMExec $ State.modify (\vmState -> vmState {timers = timersUpdate (timers vmState)})

getOpCodeBin :: VMExec stackSize OpCodeBin
getOpCodeBin = do
  vmState <- VMExec State.get
  let currentPC = pc vmState
  case addOne currentPC of
    Nothing -> VMExec $ Except.throwError "program counter (pc) is misaligned; pc + 1 is out of the address range"
    Just currentPCPlusOne ->
      withMemory $ \memoryState -> do
        -- opcodes stored big-endian
        op0 <- fmap fromIntegral $ Memory.readMemory memoryState currentPC
        op1 <- fmap fromIntegral $ Memory.readMemory memoryState currentPCPlusOne
        pure $ unsafeShiftL op0 8 .|. op1

incrementPC :: VMExec stackSize ()
incrementPC = do
  currentPC <- VMExec $ State.gets pc
  case addTwo currentPC of
    Nothing -> VMExec $ Except.throwError "incremented past end of vm memory"
    Just nextPC -> setPC nextPC

setPC :: ProgramCounter -> VMExec stackSize ()
setPC nextPC = VMExec $ State.modify (\vmState -> vmState {pc = nextPC})

withMemory :: (Memory -> IO a) -> VMExec stackSize a
withMemory memoryAction = VMExec $ State.gets memory >>= State.liftIO . memoryAction

getsRegisters :: (Registers -> a) -> VMExec stackSize a
getsRegisters projectRegisters = VMExec $ State.gets (projectRegisters . registers)

withRegisters :: (Registers -> IO a) -> VMExec stackSize a
withRegisters registersAction = VMExec $ State.gets registers >>= State.liftIO . registersAction

modifyRegisters :: (Registers -> Registers) -> VMExec stackSize ()
modifyRegisters registersUpdate = VMExec $ State.modify (\vmState -> vmState {registers = registersUpdate (registers vmState)})

setNextStackAddr :: State.MonadState (VMState stackSize) m => StackAddress stackSize -> m ()
setNextStackAddr newNextStackAddr = State.modify (\vmState -> vmState {stack = (stack vmState) {nextStackAddr = newNextStackAddr}})

addOne :: (TypeNats.KnownNat n, n <= n + 2) => Finite n -> Maybe (Finite n)
addOne n = Finite.strengthenN $ Finite.add n one

addTwo :: (TypeNats.KnownNat n, n <= n + 3) => Finite n -> Maybe (Finite n)
addTwo n = Finite.strengthenN $ Finite.add n two

one :: Finite 2
one = Finite.finite 1

two :: Finite 3
two = Finite.finite 2
