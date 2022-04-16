{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module VMState
  ( VMExec,
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
  )
where

import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Primitive (PrimState)
import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as State
import Data.Finite (Finite)
import qualified Data.Finite as Finite
import qualified Data.Vector.Unboxed.Mutable.Sized as MVector
import qualified Data.Vector.Unboxed.Sized as Vector
import Data.Word (Word16, Word8)
import GHC.TypeNats (KnownNat)

type MemorySize = 4096

type NumRegisters = 16

type MemoryData = MVector.MVector MemorySize (PrimState IO) Word8

type VRegistersData = MVector.MVector NumRegisters (PrimState IO) Word8

type MemoryAddress = Finite MemorySize

type VRegisterAddress = Finite NumRegisters

type ROMAddress = Word16

type OpCodeBin = Word16

type ProgramCounter programSize = Finite programSize

type StackAddress stackSize = Finite stackSize

newtype Memory = Memory {memData :: MemoryData}

data Registers = Registers {vRegsData :: VRegistersData, addrReg :: {-# UNPACK #-} !ROMAddress}

data Stack stackSize = Stack {stackData :: MVector.MVector stackSize (PrimState IO) ROMAddress, nextStackAddr :: !(StackAddress stackSize)}

data Timers = Timers {delay :: {-# UNPACK #-} !Word8, sound :: {-# UNPACK #-} !Word8}

data Program programSize = Program {rom :: Vector.Vector programSize OpCodeBin, pc :: !(ProgramCounter programSize)}

data VMState stackSize programSize = VMState {memory :: Memory, stack :: Stack stackSize, registers :: Registers, timers :: Timers, program :: Program programSize}

newtype VMExec stackSize programSize a = VMExec (StateT (VMState stackSize programSize) (ExceptT String IO) a) deriving (Functor, Applicative, Monad)

readMemory :: MemoryAddress -> VMExec stackSize programSize Word8
readMemory memAddr = withMemoryData $ \memoryData -> MVector.read memoryData memAddr

writeMemory :: MemoryAddress -> Word8 -> VMExec stackSize programSize ()
writeMemory memAddr byte = withMemoryData $ \memoryData -> MVector.write memoryData memAddr byte

readVRegister :: VRegisterAddress -> VMExec stackSize programSize Word8
readVRegister regNumber = withVRegistersData $ \vRegistersData -> MVector.read vRegistersData regNumber

writeVRegister :: VRegisterAddress -> Word8 -> VMExec stackSize programSize ()
writeVRegister regNumber byte = withVRegistersData $ \vRegistersData -> MVector.write vRegistersData regNumber byte

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
        romAddress <- State.liftIO $ MVector.read (stackData stackState) stackLastElemAddr
        setNextStackAddr stackLastElemAddr
        pure romAddress

pushStack :: KnownNat stackSize => ROMAddress -> VMExec stackSize programSize ()
pushStack returnAddr =
  VMExec $ do
    stackState <- State.gets stack
    let newStackLastElemAddr = nextStackAddr stackState
    case Finite.strengthen $ Finite.add newStackLastElemAddr one of
      Nothing -> Except.throwError "stack overflow"
      Just newStackNextElemAddr -> do
        State.liftIO $ MVector.write (stackData stackState) newStackLastElemAddr returnAddr
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
  let opCodeAccessor programState = Vector.index (rom programState) (pc programState)
   in VMExec $ State.gets (opCodeAccessor . program)

withMemoryData :: (MemoryData -> IO a) -> VMExec stackSize programSize a
withMemoryData memoryAction = VMExec $ State.gets (memData . memory) >>= State.liftIO . memoryAction

withVRegistersData :: (VRegistersData -> IO a) -> VMExec stackSize programSize a
withVRegistersData vRegistersAction = VMExec $ State.gets (vRegsData . registers) >>= State.liftIO . vRegistersAction

setNextStackAddr :: State.MonadState (VMState stackSize programSize) m => StackAddress stackSize -> m ()
setNextStackAddr newNextStackAddr = State.modify (\vmState -> vmState {stack = (stack vmState) {nextStackAddr = newNextStackAddr}})

modifyTimers :: (Timers -> Timers) -> VMExec stackSize programSize ()
modifyTimers timersUpdate = VMExec $ State.modify (\vmState -> vmState {timers = timersUpdate (timers vmState)})

one :: Finite 1
one = Finite.finite 1
