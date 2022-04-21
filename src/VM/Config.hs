module VM.Config where

import Data.ByteString (ByteString)
import VM.MachineCallbacks (MachineCallbacks)

data Config = Config
  { machineCallbacks :: MachineCallbacks,
    maxStackSize :: Int,
    programRom :: ByteString,
    shouldLog :: Bool
  }
