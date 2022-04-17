{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VMState.Timers
  ( Timers,
    TimersAction,
    runTimersAction,
    newTimers,
    getDelayTimer,
    setDelayTimer,
    getSoundTimer,
    setSoundTimer,
    tickTimers,
  )
where

import qualified Control.Monad.State.Strict as MTL
import Data.Word (Word8)

data Timers = Timers {delay :: {-# UNPACK #-} !Word8, sound :: {-# UNPACK #-} !Word8}

newtype TimersAction a = TimersAction (MTL.State Timers a) deriving (Functor, Applicative, Monad)

runTimersAction :: TimersAction a -> Timers -> (a, Timers)
runTimersAction (TimersAction action) timers = MTL.runState action timers

newTimers :: Timers
newTimers = Timers {delay = 0, sound = 0}

getDelayTimer :: TimersAction Word8
getDelayTimer = TimersAction $ MTL.gets delay

setDelayTimer :: Word8 -> TimersAction ()
setDelayTimer timerValue = TimersAction $ MTL.modify (\timers -> timers {delay = timerValue})

getSoundTimer :: TimersAction Word8
getSoundTimer = TimersAction $ MTL.gets sound

setSoundTimer :: Word8 -> TimersAction ()
setSoundTimer timerValue = TimersAction $ MTL.modify (\timers -> timers {sound = timerValue})

tickTimers :: TimersAction ()
tickTimers = TimersAction $ MTL.modify (\(Timers thisDelay thisSound) -> Timers (decrement thisDelay) (decrement thisSound))

decrement :: Word8 -> Word8
decrement byte = if byte == 0 then byte else byte - 1
