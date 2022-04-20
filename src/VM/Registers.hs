{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VM.Registers
  ( Registers,
    Action,
    runAction,
    dumpState,
    newRegisters,
    readVRegister,
    writeVRegister,
    modifyVRegister,
    readAddrRegister,
    writeAddrRegister,
  )
where

import BaseTypes
import Control.Monad (forM_)
import Control.Monad.Primitive (PrimState)
import qualified Control.Monad.State as MTL
import qualified Data.Finite as Finite
import qualified Data.Vector.Unboxed.Mutable.Sized as SizedMVector
import Data.Word (Word8)
import qualified ShowHelpers

type VRegistersData = SizedMVector.MVector NumRegisters (PrimState IO) Word8

data Registers = Registers {vRegsData :: VRegistersData, addrReg :: MemoryAddress}

newtype Action a = Action (MTL.StateT Registers IO a) deriving (Functor, Applicative, Monad)

runAction :: Action a -> Registers -> IO (a, Registers)
runAction (Action action) registers = MTL.runStateT action registers

dumpState :: Registers -> IO ()
dumpState registers = do
  putStrLn "Registers:"
  putStrLn "  VRegisters:"
  putStrLn "    0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a 0x0b 0x0c 0x0d 0x0e 0x0f"
  putStr "   "
  forM_ Finite.finites $ \addr -> do
    addrValue <- SizedMVector.read (vRegsData registers) addr
    putStr $ " " <> ShowHelpers.showWord8 addrValue
  putChar '\n'
  putStrLn $ "  AddressRegister: " <> ShowHelpers.showMemoryAddress (addrReg registers)

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
