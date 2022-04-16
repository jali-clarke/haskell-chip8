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
  )
where

import qualified Control.Monad.Except as Except
import Control.Monad.Primitive (PrimState)
import qualified Control.Monad.State.Strict as State
import Data.Finite (Finite)
import qualified Data.Vector.Unboxed.Mutable.Sized as MVector
import qualified Data.Vector.Unboxed.Sized as Vector
import Data.Word (Word16, Word8)

type MemorySize = 4096

type NumRegisters = 16

newtype Memory = Memory {memData :: MVector.MVector MemorySize (PrimState IO) Word8}

data Registers = Registers {vRegsData :: MVector.MVector NumRegisters (PrimState IO) Word8, addrReg :: {-# UNPACK #-} !Word16}

data Stack n = Stack {stackData :: MVector.MVector n (PrimState IO) Word16, stackPtr :: !(Finite n)}

data Timers = Timers {delay :: {-# UNPACK #-} !Word8, sound :: {-# UNPACK #-} !Word8}

data Program n = Program {rom :: Vector.Vector n Word16, pc :: !(Finite n)}

data VMState stackSize programSize = VMState {memory :: Memory, stack :: Stack stackSize, registers :: Registers, timers :: Timers, program :: Program programSize}

newtype VMExec stackSize programSize a = VMExec (State.StateT (VMState stackSize programSize) (Except.ExceptT String IO) a) deriving (Functor, Applicative, Monad)

readMemory :: Finite MemorySize -> VMExec stackSize programSize Word8
readMemory memAddr = withMemoryData $ \memoryData -> MVector.read memoryData memAddr

writeMemory :: Finite MemorySize -> Word8 -> VMExec stackSize programSize ()
writeMemory memAddr byte = withMemoryData $ \memoryData -> MVector.write memoryData memAddr byte

readVRegister :: Finite NumRegisters -> VMExec stackSize programSize Word8
readVRegister regNumber = withVRegistersData $ \vRegistersData -> MVector.read vRegistersData regNumber

writeVRegister :: Finite NumRegisters -> Word8 -> VMExec stackSize programSize ()
writeVRegister regNumber byte = withVRegistersData $ \vRegistersData -> MVector.write vRegistersData regNumber byte

readAddrRegister :: VMExec stackSize programSize Word16
readAddrRegister = VMExec $ State.gets (addrReg . registers)

writeAddrRegister :: Word16 -> VMExec stackSize programSize ()
writeAddrRegister addrValue = VMExec $ State.modify (\vmState -> vmState {registers = (registers vmState) {addrReg = addrValue}})

withMemoryData :: (MVector.MVector MemorySize (PrimState IO) Word8 -> IO a) -> VMExec stackSize programSize a
withMemoryData memoryAction = VMExec $ State.gets (memData . memory) >>= State.liftIO . memoryAction

withVRegistersData :: (MVector.MVector NumRegisters (PrimState IO) Word8 -> IO a) -> VMExec stackSize programSize a
withVRegistersData vRegistersAction = VMExec $ State.gets (vRegsData . registers) >>= State.liftIO . vRegistersAction
