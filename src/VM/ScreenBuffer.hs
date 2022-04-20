{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module VM.ScreenBuffer
  ( ScreenBuffer,
    Action,
    runAction,
    newScreenBuffer,
    setPixelOn,
    clearBuffer,
    frozenBufferData,
  )
where

import BaseTypes
import Control.Monad.Primitive (PrimState)
import qualified Control.Monad.Reader as MTL
import Data.Finite (Finite)
import qualified Data.Finite as Finite
import qualified Data.Vector.Unboxed.Mutable.Sized as SizedMVector
import qualified Data.Vector.Unboxed.Sized as SizedVector
import GHC.TypeNats (type (+))

type ScreenBufferData = SizedMVector.MVector ScreenBufferSize (PrimState IO) Bool

newtype ScreenBuffer = ScreenBuffer ScreenBufferData

newtype Action a = Action (MTL.ReaderT ScreenBuffer IO a) deriving (Functor, Applicative, Monad)

runAction :: Action a -> ScreenBuffer -> IO a
runAction (Action action) buffer = MTL.runReaderT action buffer

newScreenBuffer :: IO ScreenBuffer
newScreenBuffer = fmap ScreenBuffer $ SizedMVector.replicate False

setPixelOn :: ScreenX -> ScreenY -> Action Bool
setPixelOn screenX screenY = do
  prevPixelValue <- getPixel screenX screenY
  setPixelRaw (toBufferAddress screenX screenY) (not prevPixelValue)
  -- if the pixel was flipped from on to off, we have a collision
  pure prevPixelValue

clearBuffer :: Action ()
clearBuffer = withBufferData $ \bufferData -> SizedMVector.set bufferData False

frozenBufferData :: Action (SizedVector.Vector ScreenBufferSize Bool)
frozenBufferData = withBufferData SizedVector.freeze

setPixelRaw :: ScreenBufferAddress -> Bool -> Action ()
setPixelRaw bufferAddress pixel = withBufferData $ \bufferData -> SizedMVector.write bufferData bufferAddress pixel

getPixel :: ScreenX -> ScreenY -> Action Bool
getPixel screenX screenY = withBufferData $ \bufferData -> SizedMVector.read bufferData (toBufferAddress screenX screenY)

toBufferAddress :: ScreenX -> ScreenY -> ScreenBufferAddress
toBufferAddress screenX screenY =
  Finite.finite $
    Finite.getFinite screenY * Finite.getFinite width + Finite.getFinite screenX

withBufferData :: (ScreenBufferData -> IO a) -> Action a
withBufferData callback =
  Action $ do
    ScreenBuffer bufferData <- MTL.ask
    MTL.liftIO $ callback bufferData

width :: Finite (ScreenWidth + 1)
width = Finite.finite 64
