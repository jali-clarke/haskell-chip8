{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module VM.Memory
  ( Memory,
    Action,
    runAction,
    dumpState,
    memoryWithLoadedProgram,
    readMemory,
    writeMemory,
    writeMemoryMultiple
  )
where

import BaseTypes
import Control.Monad (forM_)
import Control.Monad.Primitive (PrimState)
import qualified Control.Monad.Except as MTL
import qualified Control.Monad.Reader as MTL
import Data.Finite (Finite)
import qualified Data.Finite as Finite
import Data.Foldable (traverse_)
import qualified Data.Vector.Unboxed.Mutable.Sized as SizedMVector
import Data.Word (Word8)
import GHC.TypeNats (type (+), type (<=))
import qualified GHC.TypeNats as TypeNats
import qualified ShowHelpers
import SizedByteString (SizedByteString)
import qualified SizedByteString
import TypeNatsHelpers

type MemoryData = SizedMVector.MVector MemorySize (PrimState IO) Word8

newtype Memory = Memory MemoryData

newtype Action a = Action (MTL.ReaderT Memory (MTL.ExceptT String IO) a) deriving (Functor, Applicative, Monad)

runAction :: Action a -> Memory -> IO (Either String a)
runAction (Action action) memory = MTL.runExceptT (MTL.runReaderT action memory)

dumpState :: Memory -> IO ()
dumpState (Memory memoryData) = do
  putStrLn "Memory:"
  putStrLn "          0x__0 0x__1 0x__2 0x__3 0x__4 0x__5 0x__6 0x__7 0x__8 0x__9 0x__a 0x__b 0x__c 0x__d 0x__e 0x__f"
  putStrLn "         ------------------------------------------------------------------------------------------------"
  memoryDataAsList <- traverse (SizedMVector.read memoryData) Finite.finites
  let rowsWithIndex = zip [0x00 .. 0xff] (ShowHelpers.rows 16 memoryDataAsList)
  forM_ rowsWithIndex $ \(rowIndex, row) ->
    putStrLn $ "  " <> ShowHelpers.showWord8 rowIndex <> "_ |" <> dumpStateRowString row

memoryWithLoadedProgram :: (TypeNats.KnownNat programSize, (programSize + 512) <= MemorySize) => SizedByteString programSize -> IO Memory
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

writeMemoryMultiple :: MemoryAddress -> [Word8] -> Action ()
writeMemoryMultiple baseMemoryAddress values =
  case values of
    [] -> pure ()
    value : rest -> do
      writeMemory baseMemoryAddress value
      case addOne baseMemoryAddress of
        Nothing ->
          if null rest
            then pure ()
            else Action $ MTL.throwError "attempted to write memory out of bounds"
        Just nextMemoryAddress -> writeMemoryMultiple nextMemoryAddress rest

writeBinFromProgram :: ((programSize + 512) <= MemorySize) => SizedByteString programSize -> Memory -> Finite programSize -> IO ()
writeBinFromProgram programRom (Memory memoryData) programAddress =
  let programByte = SizedByteString.byteAt programRom programAddress
      memoryAddress = Finite.finite (Finite.getFinite programAddress + 512)
   in SizedMVector.write memoryData memoryAddress programByte

dumpStateRowString :: [Word8] -> String
dumpStateRowString = concat . fmap (\byte -> "  " <> ShowHelpers.showWord8 byte)
