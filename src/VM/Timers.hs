{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VM.Timers
  ( Timers,
    Action,
    runAction,
    dumpState,
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
import qualified Numeric

data Timers = Timers {delay :: {-# UNPACK #-} !Word8, sound :: {-# UNPACK #-} !Word8}

newtype Action a = Action (MTL.State Timers a) deriving (Functor, Applicative, Monad)

runAction :: Action a -> Timers -> (a, Timers)
runAction (Action action) timers = MTL.runState action timers

dumpState :: Timers -> IO ()
dumpState timers = do
  putStrLn "Timers: "
  putStrLn $ "delay = 0x" <> Numeric.showHex (delay timers) "" <> " ; sound = 0x" <> Numeric.showHex (sound timers) ""

newTimers :: Timers
newTimers = Timers {delay = 0, sound = 0}

getDelayTimer :: Action Word8
getDelayTimer = Action $ MTL.gets delay

setDelayTimer :: Word8 -> Action ()
setDelayTimer timerValue = Action $ MTL.modify (\timers -> timers {delay = timerValue})

getSoundTimer :: Action Word8
getSoundTimer = Action $ MTL.gets sound

setSoundTimer :: Word8 -> Action ()
setSoundTimer timerValue = Action $ MTL.modify (\timers -> timers {sound = timerValue})

tickTimers :: Action ()
tickTimers = Action $ MTL.modify (\(Timers thisDelay thisSound) -> Timers (decrement thisDelay) (decrement thisSound))

decrement :: Word8 -> Word8
decrement byte = if byte == 0 then byte else byte - 1
