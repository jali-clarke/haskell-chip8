{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module VMState.Memory
  ( Memory,
    memoryDataWithLoadedProg,
    readMemory,
    writeMemory,
  )
where

import BaseTypes
import Control.Monad.Primitive (PrimState)
import Data.Finite (Finite)
import qualified Data.Finite as Finite
import Data.Foldable (traverse_)
import qualified Data.Vector.Unboxed.Mutable.Sized as SizedMVector
import Data.Word (Word8)
import GHC.TypeNats (type (<=))
import qualified GHC.TypeNats as TypeNats
import SizedByteString (SizedByteString)
import qualified SizedByteString

type MemoryData = SizedMVector.MVector MemorySize (PrimState IO) Word8

newtype Memory = Memory MemoryData

readMemory :: Memory -> MemoryAddress -> IO Word8
readMemory (Memory memData) memAddr = SizedMVector.read memData memAddr

writeMemory :: Memory -> MemoryAddress -> Word8 -> IO ()
writeMemory (Memory memData) memAddr byte = SizedMVector.write memData memAddr byte

memoryDataWithLoadedProg :: (TypeNats.KnownNat programSize, programSize <= MemorySize) => SizedByteString programSize -> IO Memory
memoryDataWithLoadedProg programRom = do
  let numOpCodes = SizedByteString.length' programRom
      addresses = Finite.finitesProxy numOpCodes
  memoryData <- SizedMVector.unsafeNew
  traverse_ (writeOpCodeBinFromProg programRom memoryData) addresses
  pure $ Memory memoryData

writeOpCodeBinFromProg :: (programSize <= MemorySize) => SizedByteString programSize -> MemoryData -> Finite programSize -> IO ()
writeOpCodeBinFromProg programRom memoryData programAddress =
  let programByte = SizedByteString.byteAt programRom programAddress
      memoryAddress = Finite.finite (Finite.getFinite programAddress)
   in SizedMVector.write memoryData memoryAddress programByte
