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

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (void)
import qualified Control.Monad.Reader as MTL
import Data.Word (Word8)
import qualified ShowHelpers

data Timers = Timers {delay :: MVar Word8, sound :: MVar Word8}

newtype Action a = Action (MTL.ReaderT Timers IO a) deriving (Functor, Applicative, Monad)

runAction :: Action a -> Timers -> IO a
runAction (Action action) timers = MTL.runReaderT action timers

dumpState :: Timers -> IO ()
dumpState timers = do
  putStrLn "Timers: "
  delayValue <- MVar.readMVar (delay timers)
  putStrLn $ "  Delay: " <> ShowHelpers.showWord8 delayValue
  soundValue <- MVar.readMVar (sound timers)
  putStrLn $ "  Sound: " <> ShowHelpers.showWord8 soundValue

newTimers :: IO Timers
newTimers = do
  delayMVar <- MVar.newMVar 0
  soundMVar <- MVar.newMVar 0
  pure $ Timers {delay = delayMVar, sound = soundMVar}

getDelayTimer :: Action Word8
getDelayTimer = withTimers $ MVar.readMVar . delay

setDelayTimer :: Word8 -> Action ()
setDelayTimer timerValue = withTimers $ \timers -> void (MVar.swapMVar (delay timers) timerValue)

getSoundTimer :: Action Word8
getSoundTimer = withTimers $ MVar.readMVar . sound

setSoundTimer :: Word8 -> Action ()
setSoundTimer timerValue = withTimers $ \timers -> void (MVar.swapMVar (sound timers) timerValue)

tickTimers :: Action ()
tickTimers =
  withTimers $ \timers -> do
    MVar.modifyMVar_ (delay timers) decrement
    MVar.modifyMVar_ (sound timers) decrement

withTimers :: (Timers -> IO a) -> Action a
withTimers callback =
  Action $ do
    timers <- MTL.ask
    MTL.liftIO $ callback timers

decrement :: Word8 -> IO Word8
decrement byte = pure $ if byte == 0 then byte else byte - 1
