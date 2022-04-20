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
import Control.Monad (forM_)
import qualified Control.Monad.Except as MTL
import Control.Monad.Primitive (PrimState)
import qualified Control.Monad.State as MTL
import qualified Data.Vector.Mutable as BoxedMVector
import qualified Data.Vector.Mutable.Sized as SizedBoxedMVector
import qualified GHC.TypeNats as TypeNats
import qualified ShowHelpers
import TypeNatsHelpers

type StackData stackSize = SizedBoxedMVector.MVector stackSize (PrimState IO) MemoryAddress

data Stack stackSize = Stack {stackData :: StackData stackSize, nextStackAddr :: StackAddress stackSize}

newtype Action stackSize a = Action (MTL.ExceptT String (MTL.StateT (Stack stackSize) IO) a) deriving (Functor, Applicative, Monad)

runAction :: Action stackSize a -> Stack stackSize -> IO (Either String a, Stack stackSize)
runAction (Action action) stack = MTL.runStateT (MTL.runExceptT action) stack

dumpState :: TypeNats.KnownNat stackSize => Stack stackSize -> IO ()
dumpState stack = do
  putStrLn "Stack (grows downwards):"
  case subOne (nextStackAddr stack) of
    Nothing -> pure ()
    Just topOfStack ->
      forM_ [0 .. topOfStack] $ \stackAddr -> do
        stackAddrData <- SizedBoxedMVector.read (stackData stack) stackAddr
        putStrLn $ "  " <> ShowHelpers.showMemoryAddress stackAddrData

withNewStack :: Int -> (forall stackSize. TypeNats.KnownNat stackSize => Stack stackSize -> IO r) -> IO r
withNewStack maxStackSize callback = do
  unsizedStackData <- BoxedMVector.unsafeNew maxStackSize
  SizedBoxedMVector.withSized unsizedStackData $ \thisStackData ->
    callback (Stack {stackData = thisStackData, nextStackAddr = 0})

popStack :: Action stackSize MemoryAddress
popStack =
  Action $ do
    stack <- MTL.get
    case subOne (nextStackAddr stack) of
      Nothing -> MTL.throwError "stack underflow"
      Just stackLastElemAddr -> do
        memAddress <- MTL.liftIO $ SizedBoxedMVector.read (stackData stack) stackLastElemAddr
        MTL.put $ stack {nextStackAddr = stackLastElemAddr}
        pure memAddress

pushStack :: TypeNats.KnownNat stackSize => MemoryAddress -> Action stackSize ()
pushStack returnAddr =
  Action $ do
    stack <- MTL.get
    let newStackLastElemAddr = nextStackAddr stack
    case addOne newStackLastElemAddr of
      Nothing -> MTL.throwError "stack overflow"
      Just newStackNextElemAddr -> do
        MTL.liftIO $ SizedBoxedMVector.write (stackData stack) newStackLastElemAddr returnAddr
        MTL.put $ stack {nextStackAddr = newStackNextElemAddr}
