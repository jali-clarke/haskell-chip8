{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module OpCode.Exec
  ( exec,
  )
where

import BaseTypes
import Control.Monad (when)
import Data.Bits (unsafeShiftL, unsafeShiftR, xor, (.&.), (.|.))
import Data.Finite (Finite)
import qualified Data.Finite as Finite
import Data.Word (Word8)
import GHC.TypeNats (type (+), type (<=))
import qualified GHC.TypeNats as TypeNats
import OpCode.Type
import qualified VMState
import qualified VMState.Registers as Registers
import qualified VMState.ScreenBuffer as ScreenBuffer
import qualified VMState.Stack as Stack
import qualified VMState.Timers as Timers

exec :: (TypeNats.KnownNat stackSize, stackSize <= stackSize + 2) => OpCode -> VMState.Action stackSize ()
exec opCode =
  case opCode of
    MachineCodeCall _ -> unimplemented opCode
    ClearDisplay -> do
      VMState.withScreenBufferAction ScreenBuffer.clearBuffer
      VMState.incrementPC
    ReturnFromSubroutine -> do
      returnAddress <- VMState.withStackAction Stack.popStack
      VMState.setPC returnAddress
      VMState.incrementPC
    JumpToAddress memoryAddress -> VMState.setPC memoryAddress
    CallSubroutine memoryAddress -> do
      currentPC <- VMState.getPC
      VMState.withStackAction $ Stack.pushStack currentPC
      VMState.setPC memoryAddress
    SkipNextIfRegisterEqualToConst registerAddress constByte -> do
      registerEqualToConst <-
        VMState.withRegistersAction $
          fmap (== constByte) (Registers.readVRegister registerAddress)
      VMState.incrementPC
      when registerEqualToConst VMState.incrementPC
    SkipNextIfRegisterNotEqualToConst registerAddress constByte -> do
      registerNotEqualToConst <-
        VMState.withRegistersAction $
          fmap (/= constByte) $ Registers.readVRegister registerAddress
      VMState.incrementPC
      when registerNotEqualToConst VMState.incrementPC
    SkipNextIfRegisterEqualToRegister registerAddress0 registerAddress1 -> do
      registersAreEqual <-
        VMState.withRegistersAction $
          (==) <$> Registers.readVRegister registerAddress0 <*> Registers.readVRegister registerAddress1
      VMState.incrementPC
      when registersAreEqual VMState.incrementPC
    SetToConst registerAddress constByte -> do
      VMState.withRegistersAction $ Registers.writeVRegister registerAddress constByte
      VMState.incrementPC
    IncrementByConst registerAddress incByte -> do
      -- no carry flag setting here
      VMState.withRegistersAction $ Registers.modifyVRegister registerAddress (+ incByte)
      VMState.incrementPC
    SetToRegister registerAddressDest registerAddressSrc -> do
      VMState.withRegistersAction $ do
        registerValue <- Registers.readVRegister registerAddressSrc
        Registers.writeVRegister registerAddressDest registerValue
      VMState.incrementPC
    OrRegisterInplace registerAddressDest registerAddressSrc -> do
      VMState.withRegistersAction $ inplaceBinaryOperation registerAddressDest registerAddressSrc (.|.)
      VMState.incrementPC
    AndRegisterInplace registerAddressDest registerAddressSrc -> do
      VMState.withRegistersAction $ inplaceBinaryOperation registerAddressDest registerAddressSrc (.&.)
      VMState.incrementPC
    XorRegisterInplace registerAddressDest registerAddressSrc -> do
      VMState.withRegistersAction $ inplaceBinaryOperation registerAddressDest registerAddressSrc xor
      VMState.incrementPC
    IncrementByRegister registerAddressDest registerAddressSrc -> do
      VMState.withRegistersAction $ inplaceBinaryOperationWithFlag registerAddressDest registerAddressSrc (+) (\old _ new -> new < old)
      VMState.incrementPC
    DecrementByRegister registerAddressDest registerAddressSrc -> do
      VMState.withRegistersAction $ inplaceBinaryOperationWithFlag registerAddressDest registerAddressSrc (-) (\old _ new -> new > old)
      VMState.incrementPC
    ShiftRight registerAddress -> do
      VMState.withRegistersAction $ do
        registerValue <- Registers.readVRegister registerAddress
        let newRegisterValue = unsafeShiftR registerValue 1
            flagRegisterValue = registerValue .&. 0x01
        Registers.writeVRegister registerAddress newRegisterValue
        Registers.writeVRegister flagRegister flagRegisterValue
      VMState.incrementPC
    DecrementByRegisterReverse registerAddressDest registerAddressSrc -> do
      VMState.withRegistersAction $ inplaceBinaryOperationWithFlag registerAddressDest registerAddressSrc (flip (-)) (\_ src new -> new > src)
      VMState.incrementPC
    ShiftLeft registerAddress -> do
      VMState.withRegistersAction $ do
        registerValue <- Registers.readVRegister registerAddress
        let newRegisterValue = unsafeShiftL registerValue 1
            flagRegisterValue = unsafeShiftR (registerValue .&. 0x80) 7
        Registers.writeVRegister registerAddress newRegisterValue
        Registers.writeVRegister flagRegister flagRegisterValue
      VMState.incrementPC
    SkipNextIfRegisterNotEqualToRegister registerAddress0 registerAddress1 -> do
      registersAreNotEqual <-
        VMState.withRegistersAction $
          (/=) <$> Registers.readVRegister registerAddress0 <*> Registers.readVRegister registerAddress1
      VMState.incrementPC
      when registersAreNotEqual VMState.incrementPC
    SetAddressRegisterToConst memoryAddress -> do
      VMState.withRegistersAction $ Registers.writeAddrRegister memoryAddress
      VMState.incrementPC
    JumpToAddressWithOffset baseMemoryAddress -> do
      registerValue <- VMState.withRegistersAction $ Registers.readVRegister v0Register
      case Finite.strengthenN $ Finite.add baseMemoryAddress (word8ToFinite registerValue) of
        Nothing -> VMState.throwVMError "attempted to jump beyond memory bounds"
        Just jumpAddress -> VMState.setPC jumpAddress
    SetToRandomWithMask registerAddress mask -> do
      randomByte <- VMState.randomByte
      VMState.withRegistersAction $ Registers.writeVRegister registerAddress (randomByte .&. mask)
      VMState.incrementPC
    DrawSpriteAtCoords _ _ _ -> unimplemented opCode
    SkipNextIfKeyPressed _ -> unimplemented opCode
    SkipNextIfKeyNotPressed _ -> unimplemented opCode
    SetToDelayTimerValue registerAddress -> do
      delayTimerValue <- VMState.withTimersAction Timers.getDelayTimer
      VMState.withRegistersAction $ Registers.writeVRegister registerAddress delayTimerValue
      VMState.incrementPC
    SetToKeyboardKey _ -> unimplemented opCode
    _ -> unimplemented opCode

-- SetDelayTimerToRegister VRegisterAddress
-- SetSoundTimerToRegister VRegisterAddress
-- IncrementAddressRegisterByRegister VRegisterAddress
-- GetLetterSpriteAddress VRegisterAddress
-- StoreBinaryCodedDecimalRep VRegisterAddress
-- DumpRegisters VRegisterAddress
-- LoadRegisters VRegisterAddress

unimplemented :: OpCode -> VMState.Action stackSize ()
unimplemented opCode = VMState.throwVMError $ "unimplemented opCode: " <> show opCode

inplaceBinaryOperation :: VRegisterAddress -> VRegisterAddress -> (Word8 -> Word8 -> Word8) -> Registers.Action ()
inplaceBinaryOperation registerAddressDest registerAddressSrc operator = do
  srcRegisterValue <- Registers.readVRegister registerAddressSrc
  Registers.modifyVRegister registerAddressDest (\destRegisterValue -> operator destRegisterValue srcRegisterValue)

inplaceBinaryOperationWithFlag :: VRegisterAddress -> VRegisterAddress -> (Word8 -> Word8 -> Word8) -> (Word8 -> Word8 -> Word8 -> Bool) -> Registers.Action ()
inplaceBinaryOperationWithFlag registerAddressDest registerAddressSrc operator shouldSetFlag = do
  srcRegisterValue <- Registers.readVRegister registerAddressSrc
  destRegisterValue <- Registers.readVRegister registerAddressDest
  let newDestRegisterValue = operator destRegisterValue srcRegisterValue
  Registers.writeVRegister registerAddressDest newDestRegisterValue
  let flagValue = if shouldSetFlag destRegisterValue srcRegisterValue newDestRegisterValue then 0x01 else 0x00
  Registers.writeVRegister flagRegister flagValue

word8ToFinite :: Word8 -> Finite 255
word8ToFinite = Finite.finite . fromIntegral

v0Register :: VRegisterAddress
v0Register = Finite.finite 0

flagRegister :: VRegisterAddress
flagRegister = Finite.finite 15
