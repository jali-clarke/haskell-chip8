{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module VMState.Memory
  ( Memory,
    Action,
    runAction,
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

newtype Action a = Action (MTL.ReaderT Memory IO a) deriving (Functor, Applicative, Monad)

runAction :: Action a -> Memory -> IO a
runAction (Action action) memory = MTL.runReaderT action memory

memoryWithLoadedProgram :: (TypeNats.KnownNat programSize, programSize <= MemorySize) => SizedByteString programSize -> IO Memory
memoryWithLoadedProgram programRom = do
  let programLength = SizedByteString.length' programRom
      addresses = Finite.finitesProxy programLength
  memory <- fmap Memory SizedMVector.unsafeNew
  traverse_ (writeBinFromProgram programRom memory) addresses
  pure memory

readMemory :: MemoryAddress -> Action Word8
readMemory memoryAddress =
  Action $ do
    Memory memoryData <- MTL.ask
    MTL.liftIO $ SizedMVector.read memoryData memoryAddress

writeMemory :: MemoryAddress -> Word8 -> Action ()
writeMemory memoryAddress byte =
  Action $ do
    Memory memoryData <- MTL.ask
    MTL.liftIO $ SizedMVector.write memoryData memoryAddress byte

writeBinFromProgram :: (programSize <= MemorySize) => SizedByteString programSize -> Memory -> Finite programSize -> IO ()
writeBinFromProgram programRom (Memory memoryData) programAddress =
  let programByte = SizedByteString.byteAt programRom programAddress
      memoryAddress = Finite.finite (Finite.getFinite programAddress)
   in SizedMVector.write memoryData memoryAddress programByte
