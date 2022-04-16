{-# LANGUAGE DataKinds #-}
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
    pushStack
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

type MemoryData =  MVector.MVector MemorySize (PrimState IO) Word8

type VRegistersData = MVector.MVector NumRegisters (PrimState IO) Word8

type MemoryAddress = Finite MemorySize

type VRegisterAddress = Finite NumRegisters

type ROMAddress = Word16

type OpCodeBin = Word16

type ProgramCounter programSize = Finite programSize

type StackPointer stackSize = Finite stackSize

newtype Memory = Memory {memData :: MemoryData}

data Registers = Registers {vRegsData :: VRegistersData, addrReg :: {-# UNPACK #-} !ROMAddress}

data Stack stackSize = Stack {stackData :: MVector.MVector stackSize (PrimState IO) ROMAddress, stackPtr :: !(StackPointer stackSize)}

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

pushStack :: KnownNat stackSize => ROMAddress -> VMExec stackSize programSize ()
pushStack returnAddr =
  VMExec $ do
    stackState <- State.gets stack
    let maybeNextPtr = Finite.add <$> pure (stackPtr stackState) <*> Finite.packFinite 1
    case maybeNextPtr >>= Finite.strengthen of
      Nothing -> Except.throwError "stack overflow"
      Just nextPtr -> do
        State.liftIO $ MVector.write (stackData stackState) nextPtr returnAddr
        State.modify (\vmState -> vmState {stack = (stack vmState) {stackPtr = nextPtr}})

withMemoryData :: (MemoryData -> IO a) -> VMExec stackSize programSize a
withMemoryData memoryAction = VMExec $ State.gets (memData . memory) >>= State.liftIO . memoryAction

withVRegistersData :: (VRegistersData -> IO a) -> VMExec stackSize programSize a
withVRegistersData vRegistersAction = VMExec $ State.gets (vRegsData . registers) >>= State.liftIO . vRegistersAction
