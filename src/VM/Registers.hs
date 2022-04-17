{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VM.Registers
  ( Registers,
    Action,
    runAction,
    newRegisters,
    readVRegister,
    writeVRegister,
    modifyVRegister,
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

newtype Action a = Action (MTL.StateT Registers IO a) deriving (Functor, Applicative, Monad)

runAction :: Action a -> Registers -> IO (a, Registers)
runAction (Action action) registers = MTL.runStateT action registers

newRegisters :: IO Registers
newRegisters = do
  vRegistersData <- SizedMVector.unsafeNew
  pure $ Registers {vRegsData = vRegistersData, addrReg = 0}

readVRegister :: VRegisterAddress -> Action Word8
readVRegister regAddr = withVRegistersData $ \vRegistersData -> SizedMVector.read vRegistersData regAddr

writeVRegister :: VRegisterAddress -> Word8 -> Action ()
writeVRegister regAddr byte = withVRegistersData $ \vRegistersData -> SizedMVector.write vRegistersData regAddr byte

modifyVRegister :: VRegisterAddress -> (Word8 -> Word8) -> Action ()
modifyVRegister regAddr modifyByte = withVRegistersData $ \vRegistersData -> SizedMVector.modify vRegistersData modifyByte regAddr

readAddrRegister :: Action MemoryAddress
readAddrRegister = Action $ fmap addrReg MTL.get

writeAddrRegister :: MemoryAddress -> Action ()
writeAddrRegister addrValue = Action $ MTL.modify (\registers -> registers {addrReg = addrValue})

withVRegistersData :: (VRegistersData -> IO a) -> Action a
withVRegistersData callback =
  Action $ do
    registers <- MTL.get
    MTL.liftIO $ callback (vRegsData registers)
