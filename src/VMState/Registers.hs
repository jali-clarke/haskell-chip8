module VMState.Registers
  ( Registers,
    newRegisters,
    readVRegister,
    writeVRegister,
    readAddrRegister,
    writeAddrRegister,
  )
where

import BaseTypes
import qualified Control.Monad.IO.Class as MTL
import Control.Monad.Primitive (PrimState)
import qualified Data.Vector.Unboxed.Mutable.Sized as SizedMVector
import Data.Word (Word8)

type VRegistersData = SizedMVector.MVector NumRegisters (PrimState IO) Word8

data Registers = Registers {vRegsData :: VRegistersData, addrReg :: MemoryAddress}

newRegisters :: IO Registers
newRegisters = do
  vRegistersData <- SizedMVector.unsafeNew
  pure $ Registers {vRegsData = vRegistersData, addrReg = 0}

readVRegister :: MTL.MonadIO m => Registers -> VRegisterAddress -> m (Word8, Registers)
readVRegister registers regAddr = do
  byte <- MTL.liftIO $ SizedMVector.read (vRegsData registers) regAddr
  pure (byte, registers)

writeVRegister :: MTL.MonadIO m => Registers -> VRegisterAddress -> Word8 -> m ((), Registers)
writeVRegister registers regAddr byte = do
  MTL.liftIO $ SizedMVector.write (vRegsData registers) regAddr byte
  pure ((), registers)

readAddrRegister :: MTL.MonadIO m => Registers -> m (MemoryAddress, Registers)
readAddrRegister registers = pure (addrReg registers, registers)

writeAddrRegister :: MTL.MonadIO m => Registers -> MemoryAddress -> m ((), Registers)
writeAddrRegister registers addrValue = pure ((), registers {addrReg = addrValue})
