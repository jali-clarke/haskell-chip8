{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module VMState
  ( VMExec,
    readMemory,
    writeMemory,
    readVRegister,
    writeVRegister,
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

data Registers = Registers {vreg :: MVector.MVector NumRegisters (PrimState IO) Word8, addrReg :: {-# UNPACK #-} !Word16}

data Stack n = Stack {stackData :: MVector.MVector n (PrimState IO) Word16, stackPtr :: !(Finite n)}

data Timers = Timers {delay :: {-# UNPACK #-} !Word8, sound :: {-# UNPACK #-} !Word8}

data Program n = Program {rom :: Vector.Vector n Word16, pc :: !(Finite n)}

data VMState stackSize programSize = VMState {memory :: Memory, stack :: Stack stackSize, registers :: Registers, timers :: Timers, program :: Program programSize}

newtype VMExec stackSize programSize a = VMExec (State.StateT (VMState stackSize programSize) (Except.ExceptT String IO) a) deriving (Functor, Applicative, Monad)

readMemory :: Finite MemorySize -> VMExec stackSize programSize Word8
readMemory memAddr = withMemory $ \stateMemory -> MVector.read (memData stateMemory) memAddr

writeMemory :: Finite MemorySize -> Word8 -> VMExec stackSize programSize ()
writeMemory memAddr byte = withMemory $ \stateMemory -> MVector.write (memData stateMemory) memAddr byte

readVRegister :: Finite NumRegisters -> VMExec stackSize programSize Word8
readVRegister regNumber = withRegisters $ \stateRegisters -> MVector.read (vreg stateRegisters) regNumber

writeVRegister :: Finite NumRegisters -> Word8 -> VMExec stackSize programSize ()
writeVRegister regNumber byte = withRegisters $ \stateRegisters -> MVector.write (vreg stateRegisters) regNumber byte

withMemory :: (Memory -> IO a) -> VMExec stackSize programSize a
withMemory memoryAction = VMExec $ State.gets memory >>= State.liftIO . memoryAction

withRegisters :: (Registers -> IO a) -> VMExec stackSize programSize a
withRegisters registersAction = VMExec $ State.gets registers >>= State.liftIO . registersAction
