{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module OpCode.Exec
  ( exec,
  )
where

import BaseTypes
import Control.Monad (when)
import Data.Bits (xor, (.&.), (.|.))
import qualified Data.Finite as Finite
import Data.Word (Word8)
import GHC.TypeNats (type (+), type (<=))
import qualified GHC.TypeNats as TypeNats
import OpCode.Type
import qualified VMState
import qualified VMState.Registers as Registers
import qualified VMState.ScreenBuffer as ScreenBuffer
import qualified VMState.Stack as Stack

exec :: (TypeNats.KnownNat stackSize, stackSize <= stackSize + 2) => OpCode -> VMState.Action stackSize ()
exec opCode =
  case opCode of
    MachineCodeCall _ -> unimplemented
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
      VMState.withRegistersAction $ inplaceBinaryOperationWithFlag registerAddressDest registerAddressSrc (+) (\old new -> new < old)
      VMState.incrementPC
    _ -> unimplemented

-- DecrementByRegister VRegisterAddress VRegisterAddress
-- ShiftRight VRegisterAddress
-- DecrementByRegisterReverse VRegisterAddress VRegisterAddress
-- ShiftLeft VRegisterAddress
-- SkipNextIfRegisterNotEqualToRegister VRegisterAddress VRegisterAddress
-- SetAddressRegisterToConst MemoryAddress
-- JumpToAddressWithOffset MemoryAddress
-- SetToRandomWithMask VRegisterAddress Word8
-- DrawSpriteAtCoords VRegisterAddress VRegisterAddress SpriteHeight
-- SkipNextIfKeyPressed VRegisterAddress
-- SkipNextIfKeyNotPressed VRegisterAddress
-- SetToDelayTimerValue VRegisterAddress
-- SetToKeyboardKey VRegisterAddress
-- SetDelayTimerToRegister VRegisterAddress
-- SetSoundTimerToRegister VRegisterAddress
-- IncrementAddressRegisterByRegister VRegisterAddress
-- GetLetterSpriteAddress VRegisterAddress
-- StoreBinaryCodedDecimalRep VRegisterAddress
-- DumpRegisters VRegisterAddress
-- LoadRegisters VRegisterAddress

unimplemented :: VMState.Action stackSize ()
unimplemented = pure ()

inplaceBinaryOperation :: VRegisterAddress -> VRegisterAddress -> (Word8 -> Word8 -> Word8) -> Registers.Action ()
inplaceBinaryOperation registerAddressDest registerAddressSrc operator = do
  srcRegisterValue <- Registers.readVRegister registerAddressSrc
  Registers.modifyVRegister registerAddressDest (\destRegisterValue -> operator destRegisterValue srcRegisterValue)

inplaceBinaryOperationWithFlag :: VRegisterAddress -> VRegisterAddress -> (Word8 -> Word8 -> Word8) -> (Word8 -> Word8 -> Bool) -> Registers.Action ()
inplaceBinaryOperationWithFlag registerAddressDest registerAddressSrc operator shouldSetFlag = do
  srcRegisterValue <- Registers.readVRegister registerAddressSrc
  destRegisterValue <- Registers.readVRegister registerAddressDest
  let newDestRegisterValue = operator destRegisterValue srcRegisterValue
  Registers.writeVRegister registerAddressDest newDestRegisterValue
  let flagValue = if shouldSetFlag destRegisterValue newDestRegisterValue then 0x01 else 0x00
  Registers.writeVRegister flagRegister flagValue

flagRegister :: VRegisterAddress
flagRegister = Finite.finite 15
