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
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (void)
import Control.Monad.Primitive (PrimState)
import qualified Control.Monad.Reader as MTL
import qualified Data.Finite as Finite
import qualified Data.Vector.Unboxed.Mutable.Sized as SizedMVector
import Data.Word (Word8)
import qualified ShowHelpers

type VRegistersData = SizedMVector.MVector NumRegisters (PrimState IO) Word8

data Registers = Registers {vRegsData :: VRegistersData, addrReg :: MVar MemoryAddress}

newtype Action a = Action (MTL.ReaderT Registers IO a) deriving (Functor, Applicative, Monad)

runAction :: Action a -> Registers -> IO a
runAction (Action action) registers = MTL.runReaderT action registers

dumpState :: Registers -> IO ()
dumpState registers = do
  putStrLn "Registers:"
  addrRegValue <- MVar.readMVar (addrReg registers)
  putStrLn $ "  AddressRegister: " <> ShowHelpers.showMemoryAddress addrRegValue
  putStrLn "  VRegisters:"
  putStrLn "     0x0  0x1  0x2  0x3  0x4  0x5  0x6  0x7  0x8  0x9  0xa  0xb  0xc  0xd  0xe  0xf"
  putStrLn "    -------------------------------------------------------------------------------"
  addrValuesList <- traverse (SizedMVector.read (vRegsData registers)) Finite.finites
  putStrLn $ "   " <> dumpVRegistersAsString addrValuesList

newRegisters :: IO Registers
newRegisters = Registers <$> SizedMVector.unsafeNew <*> MVar.newMVar 0

readVRegister :: VRegisterAddress -> Action Word8
readVRegister regAddr = withVRegistersData $ \vRegistersData -> SizedMVector.read vRegistersData regAddr

writeVRegister :: VRegisterAddress -> Word8 -> Action ()
writeVRegister regAddr byte = withVRegistersData $ \vRegistersData -> SizedMVector.write vRegistersData regAddr byte

modifyVRegister :: VRegisterAddress -> (Word8 -> Word8) -> Action ()
modifyVRegister regAddr modifyByte = withVRegistersData $ \vRegistersData -> SizedMVector.modify vRegistersData modifyByte regAddr

readAddrRegister :: Action MemoryAddress
readAddrRegister = withAddrRegisterMVar MVar.readMVar

writeAddrRegister :: MemoryAddress -> Action ()
writeAddrRegister addrValue = withAddrRegisterMVar $ \addrRegMVar -> void (MVar.swapMVar addrRegMVar addrValue)

withVRegistersData :: (VRegistersData -> IO a) -> Action a
withVRegistersData callback =
  Action $ do
    vRegs <- MTL.asks vRegsData
    MTL.liftIO $ callback vRegs

withAddrRegisterMVar :: (MVar MemoryAddress -> IO a) -> Action a
withAddrRegisterMVar callback =
  Action $ do
    addrVar <- MTL.asks addrReg
    MTL.liftIO $ callback addrVar

dumpVRegistersAsString :: [Word8] -> String
dumpVRegistersAsString = concat . fmap (\byte -> " " <> ShowHelpers.showWord8 byte)
