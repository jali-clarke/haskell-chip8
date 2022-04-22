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
import qualified Control.Monad.Reader as MTL
import Data.Word (Word8)
import qualified ShowHelpers

data Timers = Timers {delay :: {-# UNPACK #-} !Word8, sound :: {-# UNPACK #-} !Word8}

newtype Action a = Action (MTL.ReaderT (MVar Timers) IO a) deriving (Functor, Applicative, Monad)

runAction :: Action a -> MVar Timers -> IO a
runAction (Action action) timersMVar = MTL.runReaderT action timersMVar

dumpState :: MVar Timers -> IO ()
dumpState timersMVar = do
  timers <- MVar.readMVar timersMVar
  putStrLn "Timers: "
  putStrLn $ "  Delay: " <> ShowHelpers.showWord8 (delay timers)
  putStrLn $ "  Sound: " <> ShowHelpers.showWord8 (sound timers)

newTimers :: IO (MVar Timers)
newTimers = MVar.newMVar $ Timers {delay = 0, sound = 0}

getDelayTimer :: Action Word8
getDelayTimer = withMVar $ fmap delay . MVar.readMVar

setDelayTimer :: Word8 -> Action ()
setDelayTimer timerValue = withMVar $ modifyTimers (\timers -> timers {delay = timerValue})

getSoundTimer :: Action Word8
getSoundTimer = withMVar $ fmap sound . MVar.readMVar

setSoundTimer :: Word8 -> Action ()
setSoundTimer timerValue = withMVar $ modifyTimers (\timers -> timers {sound = timerValue})

tickTimers :: Action ()
tickTimers = withMVar $ modifyTimers (\(Timers thisDelay thisSound) -> Timers (decrement thisDelay) (decrement thisSound))

withMVar :: (MVar Timers -> IO a) -> Action a
withMVar callback =
  Action $ do
    timersMVar <- MTL.ask
    MTL.liftIO $ callback timersMVar

modifyTimers :: (Timers -> Timers) -> MVar Timers -> IO ()
modifyTimers f timersMVar = do
  timers <- MVar.takeMVar timersMVar
  MVar.putMVar timersMVar (f timers)

decrement :: Word8 -> Word8
decrement byte = if byte == 0 then byte else byte - 1
