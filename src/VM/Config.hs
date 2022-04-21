module VM.Config where

import Data.ByteString (ByteString)
import VM.Platform (Platform)

data Config = Config
  { platform :: Platform,
    maxStackSize :: Int,
    programRom :: ByteString,
    shouldLog :: Bool
  }
