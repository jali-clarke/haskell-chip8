module VM.MachineCallbacks (
  MachineCallbacks(..)
) where

import Data.Word (Word8)

data MachineCallbacks = MachineCallbacks {randomByte :: IO Word8}
