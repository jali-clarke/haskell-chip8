{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}

module VMState.ScreenBuffer (
  ScreenBuffer,
  Action,
  runAction,
  newScreenBuffer,
  setPixel,
) where

import BaseTypes
import qualified Control.Monad.Reader as MTL
import Control.Monad.Primitive (PrimState)
import Data.Finite (Finite)
import qualified Data.Finite as Finite
import qualified Data.Vector.Unboxed.Mutable.Sized as SizedMVector
import GHC.TypeNats (type (+), type (*))

type ScreenBufferSize = ScreenWidth * ScreenHeight

type ScreenBufferAddress = Finite ScreenBufferSize

type ScreenBufferData = SizedMVector.MVector ScreenBufferSize (PrimState IO) Bool

newtype ScreenBuffer = ScreenBuffer ScreenBufferData

newtype Action a = Action (MTL.ReaderT ScreenBuffer IO a) deriving (Functor, Applicative, Monad)

runAction :: Action a -> ScreenBuffer -> IO a
runAction (Action action) buffer = MTL.runReaderT action buffer

newScreenBuffer :: IO ScreenBuffer
newScreenBuffer = fmap ScreenBuffer SizedMVector.unsafeNew

setPixel :: ScreenX -> ScreenY -> Bool -> Action Bool
setPixel screenX screenY pixel = do
  currentPixel <- getPixel screenX screenY
  -- (/=) is xor
  let newPixel = currentPixel /= pixel
  Action $ do
    ScreenBuffer bufferData <- MTL.ask
    MTL.liftIO $ SizedMVector.write bufferData (toBufferAddress screenX screenY) newPixel
  -- if the pixel was flipped from on to off, we have a collision
  pure $ currentPixel && not newPixel

getPixel :: ScreenX -> ScreenY -> Action Bool
getPixel screenX screenY =
  Action $ do
    ScreenBuffer bufferData <- MTL.ask
    MTL.liftIO $ SizedMVector.read bufferData (toBufferAddress screenX screenY)

toBufferAddress :: ScreenX -> ScreenY -> ScreenBufferAddress
toBufferAddress screenX screenY =
  Finite.finite $
    Finite.getFinite screenY * Finite.getFinite width + Finite.getFinite screenX

width :: Finite (ScreenWidth + 1)
width = Finite.finite 64
