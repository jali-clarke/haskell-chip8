module Platform.Terminal
  ( platform,
  )
where

import BaseTypes
import Control.Monad (forM_)
import qualified Data.Vector.Unboxed.Sized as SizedVector
import qualified ShowHelpers
import qualified System.Console.ANSI as ANSI
import VM.Platform (Platform)
import qualified VM.Platform as Platform

platform :: Platform
platform =
  Platform.stubPlatform
    { Platform.renderFrozenScreenBufferData = renderToTerminal
    }

renderToTerminal :: SizedVector.Vector ScreenBufferSize Bool -> IO ()
renderToTerminal screenData = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
  let rows = ShowHelpers.rows 64 (SizedVector.toList screenData)
  forM_ rows $ \row ->
    putStrLn $ fmap (\b -> if b then '#' else ' ') row <> "\n"
