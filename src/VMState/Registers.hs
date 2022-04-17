{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VMState.Registers
  ( Registers,
    RegistersAction,
    newRegisters,
    runRegistersAction,
    readVRegister,
    writeVRegister,
    readAddrRegister,
    writeAddrRegister,
  )
where

import BaseTypes
import Control.Monad.Primitive (PrimState)
import qualified Control.Monad.State as MTL
import qualified Data.Vector.Unboxed.Mutable.Sized as SizedMVector
import Data.Word (Word8)

type VRegistersData = SizedMVector.MVector NumRegisters (PrimState IO) Word8

data Registers = Registers {vRegsData :: VRegistersData, addrReg :: MemoryAddress}

newtype RegistersAction a = RegistersAction (MTL.StateT Registers IO a) deriving (Functor, Applicative, Monad)

runRegistersAction :: RegistersAction a -> Registers -> IO (a, Registers)
runRegistersAction (RegistersAction action) registers = MTL.runStateT action registers

newRegisters :: IO Registers
newRegisters = do
  vRegistersData <- SizedMVector.unsafeNew
  pure $ Registers {vRegsData = vRegistersData, addrReg = 0}

readVRegister :: VRegisterAddress -> RegistersAction Word8
readVRegister regAddr =
  RegistersAction $ do
    registers <- MTL.get
    MTL.liftIO $ SizedMVector.read (vRegsData registers) regAddr

writeVRegister :: VRegisterAddress -> Word8 -> RegistersAction ()
writeVRegister regAddr byte =
  RegistersAction $ do
    registers <- MTL.get
    MTL.liftIO $ SizedMVector.write (vRegsData registers) regAddr byte

readAddrRegister :: RegistersAction MemoryAddress
readAddrRegister = RegistersAction $ fmap addrReg MTL.get

writeAddrRegister :: MemoryAddress -> RegistersAction ()
writeAddrRegister addrValue = RegistersAction $ MTL.modify (\registers -> registers {addrReg = addrValue})
