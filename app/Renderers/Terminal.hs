module Renderers.Terminal
  ( renderToTerminal,
  )
where

import BaseTypes
import Control.Monad (forM_)
import qualified Data.Vector.Unboxed.Sized as SizedVector
import qualified ShowHelpers
import qualified System.Console.ANSI as ANSI

renderToTerminal :: SizedVector.Vector ScreenBufferSize Bool -> IO ()
renderToTerminal screenData = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
  let rows = ShowHelpers.rows 64 (SizedVector.toList screenData)
  forM_ rows $ \row ->
    putStrLn $ fmap (\b -> if b then '#' else ' ') row <> "\n"
