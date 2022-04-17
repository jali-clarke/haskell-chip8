{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module VMState.Memory
  ( Memory,
    memoryWithLoadedProgram,
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

memoryWithLoadedProgram :: (TypeNats.KnownNat programSize, programSize <= MemorySize) => SizedByteString programSize -> IO Memory
memoryWithLoadedProgram programRom = do
  let programLength = SizedByteString.length' programRom
      addresses = Finite.finitesProxy programLength
  memory <- fmap Memory SizedMVector.unsafeNew
  traverse_ (writeBinFromProgram programRom memory) addresses
  pure memory

writeBinFromProgram :: (programSize <= MemorySize) => SizedByteString programSize -> Memory -> Finite programSize -> IO ()
writeBinFromProgram programRom (Memory memoryData) programAddress =
  let programByte = SizedByteString.byteAt programRom programAddress
      memoryAddress = Finite.finite (Finite.getFinite programAddress)
   in SizedMVector.write memoryData memoryAddress programByte
