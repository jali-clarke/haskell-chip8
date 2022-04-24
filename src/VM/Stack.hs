{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module VM.Stack
  ( Stack,
    Action,
    runAction,
    dumpState,
    withNewStack,
    popStack,
    pushStack,
  )
where

import BaseTypes
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (forM_, void)
import qualified Control.Monad.Except as MTL
import Control.Monad.Primitive (PrimState)
import qualified Control.Monad.Reader as MTL
import qualified Data.Vector.Mutable as BoxedMVector
import qualified Data.Vector.Mutable.Sized as SizedBoxedMVector
import qualified GHC.TypeNats as TypeNats
import qualified ShowHelpers
import TypeNatsHelpers

type StackData stackSize = SizedBoxedMVector.MVector stackSize (PrimState IO) MemoryAddress

data Stack stackSize = Stack {stackData :: StackData stackSize, nextStackAddr :: MVar (StackAddress stackSize)}

newtype Action stackSize a = Action (MTL.ReaderT (Stack stackSize) (MTL.ExceptT String IO) a) deriving (Functor, Applicative, Monad)

runAction :: Action stackSize a -> Stack stackSize -> IO (Either String a)
runAction (Action action) stack = MTL.runExceptT (MTL.runReaderT action stack)

dumpState :: TypeNats.KnownNat stackSize => Stack stackSize -> IO ()
dumpState stack = do
  putStrLn "Stack (grows downwards):"
  thisNextStackAddr <- MVar.readMVar (nextStackAddr stack)
  case subOne thisNextStackAddr of
    Nothing -> putStrLn "  <empty>"
    Just topOfStack ->
      forM_ [0 .. topOfStack] $ \stackAddr -> do
        stackAddrData <- SizedBoxedMVector.read (stackData stack) stackAddr
        putStrLn $ "  " <> ShowHelpers.showMemoryAddress stackAddrData

withNewStack :: Int -> (forall stackSize. TypeNats.KnownNat stackSize => Stack stackSize -> IO r) -> IO r
withNewStack maxStackSize callback = do
  unsizedStackData <- BoxedMVector.unsafeNew maxStackSize
  SizedBoxedMVector.withSized unsizedStackData $ \thisStackData -> do
    nextStackAddrMVar <- MVar.newMVar 0
    callback (Stack {stackData = thisStackData, nextStackAddr = nextStackAddrMVar})

popStack :: Action stackSize MemoryAddress
popStack =
  Action $ do
    stack <- MTL.ask
    thisNextStackAddr <- MTL.liftIO $ MVar.readMVar (nextStackAddr stack)
    case subOne thisNextStackAddr of
      Nothing -> MTL.throwError "stack underflow"
      Just stackLastElemAddr ->
        MTL.liftIO $ do
          memAddress <- SizedBoxedMVector.read (stackData stack) stackLastElemAddr
          void $ MVar.swapMVar (nextStackAddr stack) stackLastElemAddr
          pure memAddress

pushStack :: TypeNats.KnownNat stackSize => MemoryAddress -> Action stackSize ()
pushStack returnAddr =
  Action $ do
    stack <- MTL.ask
    newStackLastElemAddr <- MTL.liftIO $ MVar.readMVar (nextStackAddr stack)
    case addOne newStackLastElemAddr of
      Nothing -> MTL.throwError "stack overflow"
      Just newStackNextElemAddr ->
        MTL.liftIO $ do
          SizedBoxedMVector.write (stackData stack) newStackLastElemAddr returnAddr
          void $ MVar.swapMVar (nextStackAddr stack) newStackNextElemAddr
