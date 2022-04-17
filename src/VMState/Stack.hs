{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module VMState.Stack
  ( Stack,
    StackAction,
    runStackAction,
    withNewStack,
    popStack,
    pushStack,
  )
where

import BaseTypes
import qualified Control.Monad.Except as MTL
import qualified Control.Monad.State as MTL
import Control.Monad.Primitive (PrimState)
import qualified Data.Finite as Finite
import qualified Data.Vector.Mutable as BoxedMVector
import qualified Data.Vector.Mutable.Sized as SizedBoxedMVector
import GHC.TypeNats (type (+), type (<=))
import qualified GHC.TypeNats as TypeNats
import TypeNatsHelpers

type StackData stackSize = SizedBoxedMVector.MVector stackSize (PrimState IO) MemoryAddress

data Stack stackSize = Stack {stackData :: StackData stackSize, nextStackAddr :: StackAddress stackSize}

newtype StackAction stackSize a = StackAction (MTL.ExceptT String (MTL.StateT (Stack stackSize) IO) a) deriving (Functor, Applicative, Monad)

runStackAction :: StackAction stackSize a -> Stack stackSize -> IO (Either String a, Stack stackSize)
runStackAction (StackAction action) stack = MTL.runStateT (MTL.runExceptT action) stack

withNewStack :: Int -> (forall stackSize. Stack stackSize -> IO r) -> IO r
withNewStack maxStackSize callback = do
  unsizedStackData <- BoxedMVector.unsafeNew maxStackSize
  SizedBoxedMVector.withSized unsizedStackData $ \thisStackData ->
    callback (Stack {stackData = thisStackData, nextStackAddr = Finite.finite 0})

popStack :: StackAction stackSize MemoryAddress
popStack =
  StackAction $ do
    stack <- MTL.get
    case Finite.sub (nextStackAddr stack) one of
      Left _ -> MTL.throwError "stack underflow"
      Right stackLastElemAddr -> do
        memAddress <- MTL.liftIO $ SizedBoxedMVector.read (stackData stack) stackLastElemAddr
        MTL.put $ stack {nextStackAddr = stackLastElemAddr}
        pure memAddress

pushStack :: (TypeNats.KnownNat stackSize, stackSize <= stackSize + 2) => MemoryAddress -> StackAction stackSize ()
pushStack returnAddr =
  StackAction $ do
    stack <- MTL.get
    let newStackLastElemAddr = nextStackAddr stack
    case addOne newStackLastElemAddr of
      Nothing -> MTL.throwError "stack overflow"
      Just newStackNextElemAddr -> do
        MTL.liftIO $ SizedBoxedMVector.write (stackData stack) newStackLastElemAddr returnAddr
        MTL.put $ stack {nextStackAddr = newStackNextElemAddr}
