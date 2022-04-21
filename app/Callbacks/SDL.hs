{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Callbacks.SDL
  ( WindowCtx,
    withWindowCtx,
  )
where

import BaseTypes
import Control.Exception (bracket)
import Data.Finite (Finite)
import Foreign.C.Types (CInt)
import GHC.TypeNats (KnownNat, type (*), type (+))
import qualified SDL
import SDL.Vect (V2 (..))

data WindowCtx = WindowCtx
  { window :: SDL.Window,
    windowRenderer :: SDL.Renderer,
    bufferRenderer :: SDL.Renderer
  }

withWindowCtx :: (WindowCtx -> IO a) -> IO a
withWindowCtx callback =
  let config =
        SDL.defaultWindow
          { SDL.windowResizable = True,
            SDL.windowInitialSize = toV2 initialWindowWidth initialWindowHeight
          }
   in bracket (SDL.createWindow "haskell-chip8" config) SDL.destroyWindow $ \thisWindow ->
        bracket (SDL.createRGBSurface (toV2 bufferSufaceWidth bufferSufaceHeight) SDL.RGB332) SDL.freeSurface $ \bufferSurface ->
          bracket (SDL.createSoftwareRenderer bufferSurface) SDL.destroyRenderer $ \bufferSurfaceRenderer -> do
            windowSurface <- SDL.getWindowSurface thisWindow
            bracket (SDL.createSoftwareRenderer windowSurface) SDL.destroyRenderer $ \windowSurfaceRenderer ->
              let windowCtx = WindowCtx {window = thisWindow, windowRenderer = windowSurfaceRenderer, bufferRenderer = bufferSurfaceRenderer}
               in callback windowCtx

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
