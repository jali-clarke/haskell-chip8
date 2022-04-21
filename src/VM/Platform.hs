module VM.Platform
  ( Platform (..),
    stubPlatform,
  )
where

import BaseTypes
import qualified Data.Vector.Unboxed.Sized as SizedVector
import Data.Word (Word8)

data Platform = Platform
  { blockingGetKeyboardKey :: IO Char,
    isKeyPressed :: Char -> IO Bool,
    randomByte :: IO Word8,
    renderFrozenScreenBufferData :: SizedVector.Vector ScreenBufferSize Bool -> IO ()
  }

stubPlatform :: Platform
stubPlatform =
  Platform
    { blockingGetKeyboardKey = pure 'f',
      isKeyPressed = \_ -> pure False,
      randomByte = pure 0,
      renderFrozenScreenBufferData = \_ -> pure ()
    }
