{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module VMState.Memory
  ( Memory,
    MemoryAction,
    runMemoryAction,
    memoryWithLoadedProgram,
    readMemory,
    writeMemory,
  )
where

import BaseTypes
import Control.Monad.Primitive (PrimState)
import qualified Control.Monad.Reader as MTL
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

newtype MemoryAction a = MemoryAction (MTL.ReaderT Memory IO a) deriving (Functor, Applicative, Monad)

runMemoryAction :: MemoryAction a -> Memory -> IO a
runMemoryAction (MemoryAction action) memory = MTL.runReaderT action memory

memoryWithLoadedProgram :: (TypeNats.KnownNat programSize, programSize <= MemorySize) => SizedByteString programSize -> IO Memory
memoryWithLoadedProgram programRom = do
  let programLength = SizedByteString.length' programRom
      addresses = Finite.finitesProxy programLength
  memory <- fmap Memory SizedMVector.unsafeNew
  traverse_ (writeBinFromProgram programRom memory) addresses
  pure memory

readMemory :: MemoryAddress -> MemoryAction Word8
readMemory memoryAddress =
  MemoryAction $ do
    Memory memoryData <- MTL.ask
    MTL.liftIO $ SizedMVector.read memoryData memoryAddress

writeMemory :: MemoryAddress -> Word8 -> MemoryAction ()
writeMemory memoryAddress byte =
  MemoryAction $ do
    Memory memoryData <- MTL.ask
    MTL.liftIO $ SizedMVector.write memoryData memoryAddress byte

writeBinFromProgram :: (programSize <= MemorySize) => SizedByteString programSize -> Memory -> Finite programSize -> IO ()
writeBinFromProgram programRom (Memory memoryData) programAddress =
  let programByte = SizedByteString.byteAt programRom programAddress
      memoryAddress = Finite.finite (Finite.getFinite programAddress)
   in SizedMVector.write memoryData memoryAddress programByte
