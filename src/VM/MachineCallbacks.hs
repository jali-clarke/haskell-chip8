module VM.MachineCallbacks
  ( MachineCallbacks (..),
  )
where

import BaseTypes
import qualified Data.Vector.Unboxed.Sized as SizedVector
import Data.Word (Word8)

data MachineCallbacks = MachineCallbacks
  { blockingGetKeyboardKey :: IO Char,
    isKeyPressed :: Char -> IO Bool,
    randomByte :: IO Word8,
    renderFrozenScreenBufferData :: SizedVector.Vector ScreenBufferSize Bool -> IO ()
  }
