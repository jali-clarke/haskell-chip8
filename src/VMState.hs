{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module VMState
  ( VMExec,
    runVM,
    readMemory,
    writeMemory,
    readVRegister,
    writeVRegister,
    readAddrRegister,
    writeAddrRegister,
    popStack,
    pushStack,
    getDelayTimer,
    setDelayTimer,
    getSoundTimer,
    setSoundTimer,
    getOpCode,
    incrementPC,
    setPC,
  )
where

import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Primitive (PrimState)
import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as State
import Data.Finite (Finite)
import qualified Data.Finite as Finite
import qualified Data.Vector.Unboxed.Mutable.Sized as SizedMVector
import qualified Data.Vector.Unboxed.Sized as SizedVector
import Data.Word (Word16, Word8)
import GHC.TypeNats (KnownNat, type (+), type (<=))

type MemorySize = 4096

type NumRegisters = 16

type ROMAddress = Word16

type OpCodeBin = Word16

type MemoryData = SizedMVector.MVector MemorySize (PrimState IO) Word8

type VRegistersData = SizedMVector.MVector NumRegisters (PrimState IO) Word8

type StackData stackSize = SizedMVector.MVector stackSize (PrimState IO) ROMAddress

type ProgramData programSize = SizedVector.Vector programSize OpCodeBin

type MemoryAddress = Finite MemorySize

type VRegisterAddress = Finite NumRegisters

type ProgramCounter programSize = Finite programSize

type StackAddress stackSize = Finite stackSize

newtype Memory = Memory {memData :: MemoryData}

data Registers = Registers {vRegsData :: VRegistersData, addrReg :: {-# UNPACK #-} !ROMAddress}

data Stack stackSize = Stack {stackData :: StackData stackSize, nextStackAddr :: !(StackAddress stackSize)}

data Timers = Timers {delay :: {-# UNPACK #-} !Word8, sound :: {-# UNPACK #-} !Word8}

data Program programSize = Program {rom :: ProgramData programSize, pc :: !(ProgramCounter programSize)}

data VMState stackSize programSize = VMState {memory :: Memory, stack :: Stack stackSize, registers :: Registers, timers :: Timers, program :: Program programSize}

newtype VMExec stackSize programSize a = VMExec (ExceptT String (StateT (VMState stackSize programSize) IO) a) deriving (Functor, Applicative, Monad)

runVM :: VMState stackSize programSize -> VMExec stackSize programSize a -> IO (Either String a, VMState stackSize programSize)
runVM vmState (VMExec action) = State.runStateT (Except.runExceptT action) vmState

readMemory :: MemoryAddress -> VMExec stackSize programSize Word8
readMemory memAddr = withMemoryData $ \memoryData -> SizedMVector.read memoryData memAddr

writeMemory :: MemoryAddress -> Word8 -> VMExec stackSize programSize ()
writeMemory memAddr byte = withMemoryData $ \memoryData -> SizedMVector.write memoryData memAddr byte

readVRegister :: VRegisterAddress -> VMExec stackSize programSize Word8
readVRegister regNumber = withVRegistersData $ \vRegistersData -> SizedMVector.read vRegistersData regNumber

writeVRegister :: VRegisterAddress -> Word8 -> VMExec stackSize programSize ()
writeVRegister regNumber byte = withVRegistersData $ \vRegistersData -> SizedMVector.write vRegistersData regNumber byte

readAddrRegister :: VMExec stackSize programSize ROMAddress
readAddrRegister = VMExec $ State.gets (addrReg . registers)

writeAddrRegister :: ROMAddress -> VMExec stackSize programSize ()
writeAddrRegister addrValue = VMExec $ State.modify (\vmState -> vmState {registers = (registers vmState) {addrReg = addrValue}})

popStack :: VMExec stackSize programSize ROMAddress
popStack =
  VMExec $ do
    stackState <- State.gets stack
    case Finite.sub (nextStackAddr stackState) one of
      Left _ -> Except.throwError "stack underflow"
      Right stackLastElemAddr -> do
        romAddress <- State.liftIO $ SizedMVector.read (stackData stackState) stackLastElemAddr
        setNextStackAddr stackLastElemAddr
        pure romAddress

pushStack :: (KnownNat stackSize, stackSize <= stackSize + 2) => ROMAddress -> VMExec stackSize programSize ()
pushStack returnAddr =
  VMExec $ do
    stackState <- State.gets stack
    let newStackLastElemAddr = nextStackAddr stackState
    case addOne newStackLastElemAddr of
      Nothing -> Except.throwError "stack overflow"
      Just newStackNextElemAddr -> do
        State.liftIO $ SizedMVector.write (stackData stackState) newStackLastElemAddr returnAddr
        setNextStackAddr newStackNextElemAddr

getDelayTimer :: VMExec stackSize programSize Word8
getDelayTimer = VMExec $ State.gets (delay . timers)

setDelayTimer :: Word8 -> VMExec stackSize programSize ()
setDelayTimer timerValue = modifyTimers (\timerState -> timerState {delay = timerValue})

getSoundTimer :: VMExec stackSize programSize Word8
getSoundTimer = VMExec $ State.gets (sound . timers)

setSoundTimer :: Word8 -> VMExec stackSize programSize ()
setSoundTimer timerValue = modifyTimers (\timerState -> timerState {sound = timerValue})

getOpCode :: VMExec stackSize programSize OpCodeBin
getOpCode =
  let opCodeAccessor programState = SizedVector.index (rom programState) (pc programState)
   in VMExec $ State.gets (opCodeAccessor . program)

incrementPC :: (KnownNat programSize, programSize <= programSize + 2) => VMExec stackSize programSize ()
incrementPC = do
  programState <- VMExec $ State.gets program
  case addOne (pc programState) of
    Nothing -> VMExec $ Except.throwError "incremented past end of program rom"
    Just nextPC -> setPC nextPC

setPC :: ProgramCounter programSize -> VMExec stackSize programSize ()
setPC nextPC = VMExec $ State.modify (\vmState -> vmState {program = (program vmState) {pc = nextPC}})

withMemoryData :: (MemoryData -> IO a) -> VMExec stackSize programSize a
withMemoryData memoryAction = VMExec $ State.gets (memData . memory) >>= State.liftIO . memoryAction

withVRegistersData :: (VRegistersData -> IO a) -> VMExec stackSize programSize a
withVRegistersData vRegistersAction = VMExec $ State.gets (vRegsData . registers) >>= State.liftIO . vRegistersAction

setNextStackAddr :: State.MonadState (VMState stackSize programSize) m => StackAddress stackSize -> m ()
setNextStackAddr newNextStackAddr = State.modify (\vmState -> vmState {stack = (stack vmState) {nextStackAddr = newNextStackAddr}})

modifyTimers :: (Timers -> Timers) -> VMExec stackSize programSize ()
modifyTimers timersUpdate = VMExec $ State.modify (\vmState -> vmState {timers = timersUpdate (timers vmState)})

addOne :: (KnownNat n, n <= n + 2) => Finite n -> Maybe (Finite n)
addOne n = Finite.strengthenN $ Finite.add n one

one :: Finite 2
one = Finite.finite 1
