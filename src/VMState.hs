{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module VMState
  ( VMState,
    VMExec,
    withNewVMState,
    runVM,
    readMemory,
    writeMemory,
    readVRegister,
    writeVRegister,
    readAddrRegister,
    writeAddrRegister,
    popStack,
    pushStack,
    getDelayTimer,
    setDelayTimer,
    getSoundTimer,
    setSoundTimer,
    getOpCodeBin,
    incrementPC,
    setPC,
  )
where

import BaseTypes
import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Primitive (PrimState)
import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as State
import Data.Bits (unsafeShiftL, (.|.))
import Data.ByteString (ByteString)
import Data.Finite (Finite)
import qualified Data.Finite as Finite
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import qualified Data.Vector.Mutable as BoxedMVector
import qualified Data.Vector.Mutable.Sized as SizedBoxedMVector
import qualified Data.Vector.Unboxed.Mutable.Sized as SizedMVector
import Data.Word (Word8)
import qualified GHC.TypeLits.Compare as TypeNats
import GHC.TypeNats (type (*), type (+), type (<=))
import qualified GHC.TypeNats as TypeNats
import SizedByteString (SizedByteString)
import qualified SizedByteString

type MemoryData = SizedMVector.MVector MemorySize (PrimState IO) Word8

type ROMData programSize = SizedByteString programSize

type VRegistersData = SizedMVector.MVector NumRegisters (PrimState IO) Word8

type StackData stackSize = SizedBoxedMVector.MVector stackSize (PrimState IO) MemoryAddress

type ProgramCounter = MemoryAddress

type StackAddress stackSize = Finite stackSize

newtype Memory = Memory {memData :: MemoryData}

data Registers = Registers {vRegsData :: VRegistersData, addrReg :: MemoryAddress}

data Stack stackSize = Stack {stackData :: StackData stackSize, nextStackAddr :: StackAddress stackSize}

data Timers = Timers {delay :: {-# UNPACK #-} !Word8, sound :: {-# UNPACK #-} !Word8}

data VMState stackSize = VMState {memory :: Memory, stack :: (Stack stackSize), registers :: Registers, timers :: Timers, pc :: MemoryAddress}

newtype VMExec stackSize a = VMExec (ExceptT String (StateT (VMState stackSize) IO) a) deriving (Functor, Applicative, Monad)

withNewVMState :: Int -> ByteString -> (forall stackSize. Either String (VMState stackSize) -> IO r) -> IO r
withNewVMState maxStackSize programRom callback =
  SizedByteString.withSized programRom $ \sizedProgramRom -> do
    let numOpCodes = SizedByteString.length' sizedProgramRom
    case TypeNats.sameNat numOpCodes (Proxy :: Proxy 0) of
      Nothing -> callback (Left "program is empty")
      Just Refl ->
        case TypeNats.isLE (double numOpCodes) (Proxy :: Proxy MemorySize) of
          Nothing -> callback (Left "program rom is too large")
          Just Refl -> do
            memoryData <- memoryDataWithLoadedProg sizedProgramRom
            unsizedStackData <- BoxedMVector.unsafeNew maxStackSize
            SizedBoxedMVector.withSized unsizedStackData $ \thisStackData -> do
              vRegistersData <- SizedMVector.unsafeNew
              let newState =
                    VMState
                      { memory = Memory memoryData,
                        stack =
                          Stack
                            { stackData = thisStackData,
                              nextStackAddr = Finite.finite 0
                            },
                        registers =
                          Registers
                            { vRegsData = vRegistersData,
                              addrReg = 0
                            },
                        timers =
                          Timers
                            { delay = 0,
                              sound = 0
                            },
                        pc = Finite.finite 0
                      }
              callback (Right newState)

runVM :: VMState stackSize -> VMExec stackSize a -> IO (Either String a, VMState stackSize)
runVM vmState (VMExec action) = State.runStateT (Except.runExceptT action) vmState

readMemory :: MemoryAddress -> VMExec stackSize Word8
readMemory memAddr = withMemoryData $ \memoryData -> SizedMVector.read memoryData memAddr

writeMemory :: MemoryAddress -> Word8 -> VMExec stackSize ()
writeMemory memAddr byte = withMemoryData $ \memoryData -> SizedMVector.write memoryData memAddr byte

readVRegister :: VRegisterAddress -> VMExec stackSize Word8
readVRegister regNumber = withVRegistersData $ \vRegistersData -> SizedMVector.read vRegistersData regNumber

writeVRegister :: VRegisterAddress -> Word8 -> VMExec stackSize ()
writeVRegister regNumber byte = withVRegistersData $ \vRegistersData -> SizedMVector.write vRegistersData regNumber byte

readAddrRegister :: VMExec stackSize MemoryAddress
readAddrRegister = VMExec $ State.gets (addrReg . registers)

writeAddrRegister :: MemoryAddress -> VMExec stackSize ()
writeAddrRegister addrValue = VMExec $ State.modify (\vmState -> vmState {registers = (registers vmState) {addrReg = addrValue}})

popStack :: VMExec stackSize MemoryAddress
popStack =
  VMExec $ do
    stackState <- State.gets stack
    case Finite.sub (nextStackAddr stackState) one of
      Left _ -> Except.throwError "stack underflow"
      Right stackLastElemAddr -> do
        memAddress <- State.liftIO $ SizedBoxedMVector.read (stackData stackState) stackLastElemAddr
        setNextStackAddr stackLastElemAddr
        pure memAddress

pushStack :: (TypeNats.KnownNat stackSize, stackSize <= stackSize + 2) => MemoryAddress -> VMExec stackSize ()
pushStack returnAddr =
  VMExec $ do
    stackState <- State.gets stack
    let newStackLastElemAddr = nextStackAddr stackState
    case addOne newStackLastElemAddr of
      Nothing -> Except.throwError "stack overflow"
      Just newStackNextElemAddr -> do
        State.liftIO $ SizedBoxedMVector.write (stackData stackState) newStackLastElemAddr returnAddr
        setNextStackAddr newStackNextElemAddr

getDelayTimer :: VMExec stackSize Word8
getDelayTimer = VMExec $ State.gets (delay . timers)

setDelayTimer :: Word8 -> VMExec stackSize ()
setDelayTimer timerValue = modifyTimers (\timerState -> timerState {delay = timerValue})

getSoundTimer :: VMExec stackSize Word8
getSoundTimer = VMExec $ State.gets (sound . timers)

setSoundTimer :: Word8 -> VMExec stackSize ()
setSoundTimer timerValue = modifyTimers (\timerState -> timerState {sound = timerValue})

getOpCodeBin :: VMExec stackSize OpCodeBin
getOpCodeBin = do
  vmState <- VMExec State.get
  let currentPC = pc vmState
  case addOne currentPC of
    Nothing -> VMExec $ Except.throwError "program counter (pc) is misaligned; pc + 1 is out of the address range"
    Just currentPCPlusOne -> do
      -- opcodes stored big-endian
      op0 <- fmap fromIntegral (readMemory currentPC)
      op1 <- fmap fromIntegral (readMemory currentPCPlusOne)
      pure $ unsafeShiftL op0 8 .|. op1

incrementPC :: VMExec stackSize ()
incrementPC = do
  currentPC <- VMExec $ State.gets pc
  case addTwo currentPC of
    Nothing -> VMExec $ Except.throwError "incremented past end of vm memory"
    Just nextPC -> setPC nextPC

setPC :: ProgramCounter -> VMExec stackSize ()
setPC nextPC = VMExec $ State.modify (\vmState -> vmState {pc = nextPC})

withMemoryData :: (MemoryData -> IO a) -> VMExec stackSize a
withMemoryData memoryAction = VMExec $ State.gets (memData . memory) >>= State.liftIO . memoryAction

withVRegistersData :: (VRegistersData -> IO a) -> VMExec stackSize a
withVRegistersData vRegistersAction = VMExec $ State.gets (vRegsData . registers) >>= State.liftIO . vRegistersAction

setNextStackAddr :: State.MonadState (VMState stackSize) m => StackAddress stackSize -> m ()
setNextStackAddr newNextStackAddr = State.modify (\vmState -> vmState {stack = (stack vmState) {nextStackAddr = newNextStackAddr}})

modifyTimers :: (Timers -> Timers) -> VMExec stackSize ()
modifyTimers timersUpdate = VMExec $ State.modify (\vmState -> vmState {timers = timersUpdate (timers vmState)})

memoryDataWithLoadedProg :: (TypeNats.KnownNat programSize, programSize <= MemorySize) => ROMData programSize -> IO MemoryData
memoryDataWithLoadedProg programRom = do
  let numOpCodes = SizedByteString.length' programRom
      addresses = Finite.finitesProxy numOpCodes
  memoryData <- SizedMVector.unsafeNew
  traverse_ (writeOpCodeBinFromProg programRom memoryData) addresses
  pure memoryData

writeOpCodeBinFromProg :: (programSize <= MemorySize) => ROMData programSize -> MemoryData -> Finite programSize -> IO ()
writeOpCodeBinFromProg programRom memoryData programAddress =
  let programByte = SizedByteString.byteAt programRom programAddress
      memoryAddress = Finite.finite (Finite.getFinite programAddress)
   in SizedMVector.write memoryData memoryAddress programByte

double :: Proxy n -> Proxy (2 * n)
double _ = Proxy

addOne :: (TypeNats.KnownNat n, n <= n + 2) => Finite n -> Maybe (Finite n)
addOne n = Finite.strengthenN $ Finite.add n one

addTwo :: (TypeNats.KnownNat n, n <= n + 3) => Finite n -> Maybe (Finite n)
addTwo n = Finite.strengthenN $ Finite.add n two

one :: Finite 2
one = Finite.finite 1

two :: Finite 3
two = Finite.finite 2
