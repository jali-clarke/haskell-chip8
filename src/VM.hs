{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module VM
  ( VMState,
    Action,
    withNewVMState,
    runAction,
    withMemoryAction,
    withRegistersAction,
    withScreenBufferAction,
    withStackAction,
    withTimersAction,
    getOpCodeBin,
    incrementPC,
    getPC,
    setPC,
    randomByte,
    getKeyboardKey,
    isKeyPressed,
    renderFrozenScreenBufferData,
    throwVMError,
    dumpState,
    debugLog,
  )
where

import BaseTypes
import Control.Monad (when)
import qualified Control.Monad.Except as MTL
import qualified Control.Monad.State.Strict as MTL
import Data.Bits (unsafeShiftL, (.|.))
import qualified Data.Finite as Finite
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import Data.Word (Word8)
import qualified GHC.TypeLits.Compare as TypeNats
import GHC.TypeLits.Witnesses (SNat (..), (%+))
import GHC.TypeNats (type (+))
import qualified GHC.TypeNats as TypeNats
import qualified ShowHelpers
import qualified SizedByteString
import TypeNatsHelpers
import VM.Config (Config)
import qualified VM.Config as Config
import VM.Memory (Memory)
import qualified VM.Memory as Memory
import VM.Platform (Platform)
import qualified VM.Platform as Platform
import VM.Registers (Registers)
import qualified VM.Registers as Registers
import VM.ScreenBuffer (ScreenBuffer)
import qualified VM.ScreenBuffer as ScreenBuffer
import VM.Stack (Stack)
import qualified VM.Stack as Stack
import VM.Timers (Timers)
import qualified VM.Timers as Timers

data VMState stackSize = VMState
  { memory :: Memory,
    registers :: Registers,
    screenBuffer :: ScreenBuffer,
    stack :: Stack stackSize,
    timers :: Timers,
    pc :: MemoryAddress,
    platform :: Platform,
    shouldLog :: Bool
  }

newtype Action stackSize a = Action (MTL.ExceptT String (MTL.StateT (VMState stackSize) IO) a) deriving (Functor, Applicative, Monad)

withNewVMState :: Config -> (forall stackSize. TypeNats.KnownNat stackSize => Either String (VMState stackSize) -> IO r) -> IO r
withNewVMState config callback =
  Stack.withNewStack (Config.maxStackSize config) $ \(newStack :: Stack thisStackSize) ->
    SizedByteString.withSized (Config.programRom config) $ \sizedProgramRom -> do
      case SizedByteString.length' sizedProgramRom of
        (byteStringSize :: Proxy programSize) ->
          case TypeNats.sameNat byteStringSize (Proxy :: Proxy 0) of
            Just Refl -> callback (Left "program is empty" :: Either String (VMState thisStackSize))
            Nothing ->
              case (SNat :: SNat programSize) %+ (SNat :: SNat 512) of
                SNat ->
                  case TypeNats.isLE (Proxy :: Proxy (programSize + 512)) (Proxy :: Proxy MemorySize) of
                    Nothing -> callback (Left "program rom is too large" :: Either String (VMState thisStackSize))
                    Just Refl -> do
                      loadedMemory <- Memory.memoryWithLoadedProgram sizedProgramRom
                      newRegisters <- Registers.newRegisters
                      newScreenBuffer <- ScreenBuffer.newScreenBuffer
                      let newState =
                            VMState
                              { platform = Config.platform config,
                                memory = loadedMemory,
                                registers = newRegisters,
                                screenBuffer = newScreenBuffer,
                                stack = newStack,
                                timers = Timers.newTimers,
                                pc = 0x200,
                                shouldLog = Config.shouldLog config
                              }
                      callback (Right newState)

runAction :: Action stackSize a -> VMState stackSize -> IO (Either String a, VMState stackSize)
runAction (Action action) vmState = MTL.runStateT (MTL.runExceptT action) vmState

withMemoryAction :: Memory.Action a -> Action stackSize a
withMemoryAction memoryAction =
  Action $ do
    thisMemory <- MTL.gets memory
    maybeResult <- MTL.liftIO $ Memory.runAction memoryAction thisMemory
    case maybeResult of
      Left err -> MTL.throwError err
      Right value -> pure value

withRegistersAction :: Registers.Action a -> Action stackSize a
withRegistersAction registersAction =
  Action $ do
    vmState <- MTL.get
    (result, newRegisters) <- MTL.liftIO $ Registers.runAction registersAction (registers vmState)
    MTL.put (vmState {registers = newRegisters})
    pure result

withScreenBufferAction :: ScreenBuffer.Action a -> Action stackSize a
withScreenBufferAction screenBufferAction =
  Action $ do
    thisScreenBuffer <- MTL.gets screenBuffer
    MTL.liftIO $ ScreenBuffer.runAction screenBufferAction thisScreenBuffer

withStackAction :: Stack.Action stackSize a -> Action stackSize a
withStackAction stackAction = do
  maybeResult <-
    Action $ do
      vmState <- MTL.get
      (maybeResult, newStack) <- MTL.liftIO $ Stack.runAction stackAction (stack vmState)
      MTL.put (vmState {stack = newStack})
      pure maybeResult
  case maybeResult of
    Left err -> throwVMError err
    Right result -> pure result

withTimersAction :: Timers.Action a -> Action stackSize a
withTimersAction timersAction =
  Action $ do
    vmState <- MTL.get
    let (result, newTimers) = Timers.runAction timersAction (timers vmState)
    MTL.put (vmState {timers = newTimers})
    pure result

getOpCodeBin :: Action stackSize OpCodeBin
getOpCodeBin = do
  vmState <- Action MTL.get
  let currentPC = pc vmState
  case addOne currentPC of
    Nothing -> throwVMError "program counter (pc) is misaligned; pc + 1 is out of the address range"
    Just currentPCPlusOne ->
      withMemoryAction $ do
        -- opcodes stored big-endian
        op0 <- fmap fromIntegral $ Memory.readMemory currentPC
        op1 <- fmap fromIntegral $ Memory.readMemory currentPCPlusOne
        pure $ unsafeShiftL op0 8 .|. op1

incrementPC :: Action stackSize ()
incrementPC = do
  currentPC <- Action $ MTL.gets pc
  case addTwo currentPC of
    Nothing -> throwVMError "incremented past end of vm memory"
    Just nextPC -> setPC nextPC

getPC :: Action stackSize MemoryAddress
getPC = Action $ MTL.gets pc

setPC :: MemoryAddress -> Action stackSize ()
setPC nextPC = Action $ MTL.modify (\vmState -> vmState {pc = nextPC})

randomByte :: Action stackSize Word8
randomByte = withPlatform Platform.randomByte

getKeyboardKey :: Action stackSize KeyboardKey
getKeyboardKey = do
  keyChar <- withPlatform Platform.blockingGetKeyboardKey
  case keyChar of
    '0' -> pure 0
    '1' -> pure 1
    '2' -> pure 2
    '3' -> pure 3
    '4' -> pure 4
    '5' -> pure 5
    '6' -> pure 6
    '7' -> pure 7
    '8' -> pure 8
    '9' -> pure 9
    'a' -> pure 10
    'b' -> pure 11
    'c' -> pure 12
    'd' -> pure 13
    'e' -> pure 14
    'f' -> pure 15
    _ -> getKeyboardKey

isKeyPressed :: KeyboardKey -> Action stackSize Bool
isKeyPressed keyboardKey =
  withPlatform $ \thisPlatform ->
    let checkKeyPressed = Platform.isKeyPressed thisPlatform
     in case Finite.getFinite keyboardKey of
          0 -> checkKeyPressed '0'
          1 -> checkKeyPressed '1'
          2 -> checkKeyPressed '2'
          3 -> checkKeyPressed '3'
          4 -> checkKeyPressed '4'
          5 -> checkKeyPressed '5'
          6 -> checkKeyPressed '6'
          7 -> checkKeyPressed '7'
          8 -> checkKeyPressed '8'
          9 -> checkKeyPressed '9'
          10 -> checkKeyPressed 'a'
          11 -> checkKeyPressed 'b'
          12 -> checkKeyPressed 'c'
          13 -> checkKeyPressed 'd'
          14 -> checkKeyPressed 'e'
          15 -> checkKeyPressed 'f'
          -- this is just here to placate the compiler - we will never reach this point
          _ -> error "VM.isKeyPressed: major bug - this should never happen"

renderFrozenScreenBufferData :: Action stackSize ()
renderFrozenScreenBufferData = do
  frozenBufferData <- withScreenBufferAction ScreenBuffer.frozenBufferData
  withPlatform $ \thisPlatform ->
    Platform.renderFrozenScreenBufferData thisPlatform frozenBufferData

throwVMError :: String -> Action stackSize a
throwVMError errMsg = Action $ MTL.throwError errMsg

dumpState :: TypeNats.KnownNat stackSize => VMState stackSize -> IO ()
dumpState vmState = do
  Timers.dumpState (timers vmState)
  putChar '\n'
  ScreenBuffer.dumpState (screenBuffer vmState)
  putChar '\n'
  Stack.dumpState (stack vmState)
  putChar '\n'
  Registers.dumpState (registers vmState)
  putChar '\n'
  putStrLn $ "Program counter: " <> ShowHelpers.showMemoryAddress (pc vmState)
  putChar '\n'
  Memory.dumpState (memory vmState)
  putChar '\n'

withPlatform :: (Platform -> IO a) -> Action stackSize a
withPlatform platformCallback =
  Action $ do
    thisPlatform <- MTL.gets platform
    MTL.liftIO $ platformCallback thisPlatform

debugLog :: String -> Action stackSize ()
debugLog message =
  Action $ do
    shouldLogMessage <- MTL.gets shouldLog
    when shouldLogMessage $
      MTL.liftIO $ putStrLn message
