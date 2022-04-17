{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}

module VMState.ScreenBuffer (
  ScreenBuffer,
  Action,
  runAction,
  newScreenBuffer
) where

import BaseTypes
import qualified Control.Monad.Reader as MTL
import Control.Monad.Primitive (PrimState)
import qualified Data.Vector.Unboxed.Mutable.Sized as SizedMVector
import GHC.TypeNats (type (*))

type ScreenBufferSize = ScreenWidth * ScreenHeight

type ScreenBufferData = SizedMVector.MVector ScreenBufferSize (PrimState IO) Bool

newtype ScreenBuffer = ScreenBuffer ScreenBufferData

newtype Action a = Action (MTL.ReaderT ScreenBuffer IO a) deriving (Functor, Applicative, Monad)

runAction :: Action a -> ScreenBuffer -> IO a
runAction (Action action) buffer = MTL.runReaderT action buffer

newScreenBuffer :: IO ScreenBuffer
newScreenBuffer = fmap ScreenBuffer SizedMVector.unsafeNew
