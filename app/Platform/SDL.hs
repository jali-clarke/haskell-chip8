{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Platform.SDL
  ( WindowCtx,
    withPlatform,
  )
where

import BaseTypes
import Control.Exception (bracket, bracket_)
import Data.Finite (Finite)
import Data.StateVar (($=))
import qualified Data.Vector.Storable as StorableVector
import qualified Data.Vector.Unboxed.Sized as SizedVector
import Foreign.C.Types (CInt)
import GHC.TypeNats (KnownNat, type (*), type (+))
import qualified SDL
import SDL.Vect (Point (..), V2 (..), V4 (..))
import VM.Platform (Platform)
import qualified VM.Platform as Platform

newtype WindowCtx = WindowCtx
  { windowRenderer :: SDL.Renderer
  }

withPlatform :: (Platform -> IO a) -> IO a
withPlatform platformCallback =
  withWindowCtx $ \windowCtx ->
    let platform =
          Platform.stubPlatform
            { Platform.renderFrozenScreenBufferData = renderWithWindowCtx windowCtx
            }
     in platformCallback platform

withWindowCtx :: (WindowCtx -> IO a) -> IO a
withWindowCtx callback =
  let config =
        SDL.defaultWindow
          { SDL.windowResizable = True,
            SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL,
            SDL.windowInitialSize = toV2 initialWindowWidth initialWindowHeight
          }
   in bracket_ (SDL.initialize [SDL.InitVideo]) SDL.quit $
        bracket (SDL.createWindow "haskell-chip8" config) SDL.destroyWindow $ \thisWindow -> do
          SDL.raiseWindow thisWindow
          bracket (SDL.createRenderer thisWindow (-1) SDL.defaultRenderer) SDL.destroyRenderer $ \thisWindowRenderer -> do
            SDL.rendererLogicalSize thisWindowRenderer $= Just (toV2 bufferSufaceWidth bufferSufaceHeight)
            callback $ WindowCtx {windowRenderer = thisWindowRenderer}

renderWithWindowCtx :: WindowCtx -> SizedVector.Vector ScreenBufferSize Bool -> IO ()
renderWithWindowCtx windowCtx bufferData =
  let thisWindowRenderer = windowRenderer windowCtx
   in do
        SDL.pumpEvents
        SDL.rendererDrawColor thisWindowRenderer $= V4 0x00 0x00 0x00 0xff
        SDL.clear thisWindowRenderer
        SDL.rendererDrawColor thisWindowRenderer $= V4 0xff 0xff 0xff 0xff
        SDL.drawPoints thisWindowRenderer (pointsToDraw bufferData)
        SDL.present thisWindowRenderer

pointsToDraw :: SizedVector.Vector ScreenBufferSize Bool -> StorableVector.Vector (SDL.Point V2 CInt)
pointsToDraw bufferData =
  let bufferDataListWithIndex = zip [0 ..] (SizedVector.toList bufferData)
      bufferDataSetPixels = filter snd bufferDataListWithIndex
      indexToV2 index = let (y, x) = index `quotRem` (fromIntegral bufferSufaceWidth) in V2 x y
   in StorableVector.fromList $ fmap (P . indexToV2 . fst) bufferDataSetPixels

toV2 :: (KnownNat width, KnownNat height) => Finite width -> Finite height -> V2 CInt
toV2 width height = V2 (fromIntegral width) (fromIntegral height)

initialWindowWidth :: Finite ((ScreenWidth * 10) + 1)
initialWindowWidth = 640

initialWindowHeight :: Finite ((ScreenHeight * 10) + 1)
initialWindowHeight = 320

bufferSufaceWidth :: Finite (ScreenWidth + 1)
bufferSufaceWidth = 64

bufferSufaceHeight :: Finite (ScreenHeight + 1)
bufferSufaceHeight = 32