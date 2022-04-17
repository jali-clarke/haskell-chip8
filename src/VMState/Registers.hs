{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VMState.Registers
  ( Registers,
    Action,
    runAction,
    newRegisters,
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

newtype Action a = Action (MTL.StateT Registers IO a) deriving (Functor, Applicative, Monad)

runAction :: Action a -> Registers -> IO (a, Registers)
runAction (Action action) registers = MTL.runStateT action registers

newRegisters :: IO Registers
newRegisters = do
  vRegistersData <- SizedMVector.unsafeNew
  pure $ Registers {vRegsData = vRegistersData, addrReg = 0}

readVRegister :: VRegisterAddress -> Action Word8
readVRegister regAddr =
  Action $ do
    registers <- MTL.get
    MTL.liftIO $ SizedMVector.read (vRegsData registers) regAddr

writeVRegister :: VRegisterAddress -> Word8 -> Action ()
writeVRegister regAddr byte =
  Action $ do
    registers <- MTL.get
    MTL.liftIO $ SizedMVector.write (vRegsData registers) regAddr byte

readAddrRegister :: Action MemoryAddress
readAddrRegister = Action $ fmap addrReg MTL.get

writeAddrRegister :: MemoryAddress -> Action ()
writeAddrRegister addrValue = Action $ MTL.modify (\registers -> registers {addrReg = addrValue})
