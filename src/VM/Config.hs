module VM.Config where

import Data.ByteString (ByteString)
import VM.Platform (Platform)

data Config = Config
  { maxStackSize :: Int,
    platform :: Platform,
    programRom :: ByteString,
    shouldLog :: Bool,
    tickRate :: Int -- microseconds
  }
