module VMState.Timers
  ( Timers,
    newTimers,
    getDelayTimer,
    setDelayTimer,
    getSoundTimer,
    setSoundTimer,
    tickTimers,
  )
where

import Data.Word (Word8)

data Timers = Timers {delay :: {-# UNPACK #-} !Word8, sound :: {-# UNPACK #-} !Word8}

newTimers :: Timers
newTimers = Timers {delay = 0, sound = 0}

getDelayTimer :: Timers -> (Word8, Timers)
getDelayTimer timers = (delay timers, timers)

setDelayTimer :: Word8 -> Timers -> ((), Timers)
setDelayTimer timerValue timers = ((), timers {delay = timerValue})

getSoundTimer :: Timers -> (Word8, Timers)
getSoundTimer timers = (sound timers, timers)

setSoundTimer :: Word8 -> Timers -> ((), Timers)
setSoundTimer timerValue timers = ((), timers {sound = timerValue})

tickTimers :: Timers -> ((), Timers)
tickTimers (Timers thisDelay thisSound) = ((), Timers (decrement thisDelay) (decrement thisSound))

decrement :: Word8 -> Word8
decrement byte = if byte == 0 then byte else byte - 1
