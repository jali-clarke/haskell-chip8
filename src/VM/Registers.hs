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
  putStrLn $ "  AddressRegister: " <> ShowHelpers.showMemoryAddress (addrReg registers)
  putStrLn "  VRegisters:"
  putStrLn "     0x0  0x1  0x2  0x3  0x4  0x5  0x6  0x7  0x8  0x9  0xa  0xb  0xc  0xd  0xe  0xf"
  putStrLn "    -------------------------------------------------------------------------------"
  addrValuesList <- traverse (SizedMVector.read (vRegsData registers)) Finite.finites
  putStrLn $ "   " <> dumpVRegistersAsString addrValuesList

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

dumpVRegistersAsString :: [Word8] -> String
dumpVRegistersAsString = concat . fmap (\byte -> " " <> ShowHelpers.showWord8 byte)
