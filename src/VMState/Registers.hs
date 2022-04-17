module VMState.Registers (
  Registers,
  newRegisters,
  readVRegister,
  writeVRegister,
  readAddrRegister,
  writeAddrRegister
) where

import BaseTypes
import Control.Monad.Primitive (PrimState)
import qualified Data.Vector.Unboxed.Mutable.Sized as SizedMVector
import Data.Word (Word8)

type VRegistersData = SizedMVector.MVector NumRegisters (PrimState IO) Word8

data Registers = Registers {vRegsData :: VRegistersData, addrReg :: MemoryAddress}

newRegisters :: IO Registers
newRegisters = do
  vRegistersData <- SizedMVector.unsafeNew
  pure $ Registers {vRegsData = vRegistersData, addrReg = 0}

readVRegister :: Registers -> VRegisterAddress -> IO Word8
readVRegister registers regAddr = SizedMVector.read (vRegsData registers) regAddr

writeVRegister :: Registers -> VRegisterAddress -> Word8 -> IO ()
writeVRegister registers regAddr byte = SizedMVector.write (vRegsData registers) regAddr byte

readAddrRegister :: Registers -> MemoryAddress
readAddrRegister = addrReg

writeAddrRegister :: Registers -> MemoryAddress -> Registers
writeAddrRegister registers addrValue = registers {addrReg = addrValue}
