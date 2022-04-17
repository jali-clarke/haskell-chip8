{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module VMState.Stack
  ( Stack,
    StackAddress,
    withNewStack,
    popStack,
    pushStack,
  )
where

import BaseTypes
import qualified Control.Monad.Except as MTL
import Control.Monad.Primitive (PrimState)
import Data.Finite (Finite)
import qualified Data.Finite as Finite
import qualified Data.Vector.Mutable as BoxedMVector
import qualified Data.Vector.Mutable.Sized as SizedBoxedMVector
import GHC.TypeNats (type (+), type (<=))
import qualified GHC.TypeNats as TypeNats
import TypeNatsHelpers

type StackData stackSize = SizedBoxedMVector.MVector stackSize (PrimState IO) MemoryAddress

type StackAddress stackSize = Finite stackSize

data Stack stackSize = Stack {stackData :: StackData stackSize, nextStackAddr :: StackAddress stackSize}

withNewStack :: Int -> (forall stackSize. Stack stackSize -> IO r) -> IO r
withNewStack maxStackSize callback = do
  unsizedStackData <- BoxedMVector.unsafeNew maxStackSize
  SizedBoxedMVector.withSized unsizedStackData $ \thisStackData ->
    callback (Stack {stackData = thisStackData, nextStackAddr = Finite.finite 0})

popStack :: (MTL.MonadIO m, MTL.MonadError String m) => Stack stackSize -> m (MemoryAddress, Stack stackSize)
popStack stack = do
  case Finite.sub (nextStackAddr stack) one of
    Left _ -> MTL.throwError "stack underflow"
    Right stackLastElemAddr -> do
      memAddress <- MTL.liftIO $ SizedBoxedMVector.read (stackData stack) stackLastElemAddr
      pure (memAddress, stack {nextStackAddr = stackLastElemAddr})

pushStack :: (TypeNats.KnownNat stackSize, stackSize <= stackSize + 2, MTL.MonadIO m, MTL.MonadError String m) => Stack stackSize -> MemoryAddress -> m ((), Stack stackSize)
pushStack stack returnAddr = do
  let newStackLastElemAddr = nextStackAddr stack
  case addOne newStackLastElemAddr of
    Nothing -> MTL.throwError "stack overflow"
    Just newStackNextElemAddr -> do
      MTL.liftIO $ SizedBoxedMVector.write (stackData stack) newStackLastElemAddr returnAddr
      pure ((), stack {nextStackAddr = newStackNextElemAddr})
