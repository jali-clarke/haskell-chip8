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
import qualified GHC.TypeNats as TypeNats
import OpCode.Type
import TypeNatsHelpers
import qualified VM
import qualified VM.Memory as Memory
import qualified VM.Registers as Registers
import qualified VM.ScreenBuffer as ScreenBuffer
import qualified VM.Stack as Stack
import qualified VM.Timers as Timers

exec :: TypeNats.KnownNat stackSize => OpCode -> VM.Action stackSize ()
exec opCode =
  case opCode of
    MachineCodeCall _ -> unimplemented opCode
    ClearDisplay -> do
      VM.withScreenBufferAction ScreenBuffer.clearBuffer
      VM.incrementPC
    ReturnFromSubroutine -> do
      returnAddress <- VM.withStackAction Stack.popStack
      VM.setPC returnAddress
      VM.incrementPC
    JumpToAddress memoryAddress -> VM.setPC memoryAddress
    CallSubroutine memoryAddress -> do
      currentPC <- VM.getPC
      VM.withStackAction $ Stack.pushStack currentPC
      VM.setPC memoryAddress
    SkipNextIfRegisterEqualToConst registerAddress constByte -> do
      registerEqualToConst <-
        VM.withRegistersAction $
          fmap (== constByte) (Registers.readVRegister registerAddress)
      VM.incrementPC
      when registerEqualToConst VM.incrementPC
    SkipNextIfRegisterNotEqualToConst registerAddress constByte -> do
      registerNotEqualToConst <-
        VM.withRegistersAction $
          fmap (/= constByte) $ Registers.readVRegister registerAddress
      VM.incrementPC
      when registerNotEqualToConst VM.incrementPC
    SkipNextIfRegisterEqualToRegister registerAddress0 registerAddress1 -> do
      registersAreEqual <-
        VM.withRegistersAction $
          (==) <$> Registers.readVRegister registerAddress0 <*> Registers.readVRegister registerAddress1
      VM.incrementPC
      when registersAreEqual VM.incrementPC
    SetToConst registerAddress constByte -> do
      VM.withRegistersAction $ Registers.writeVRegister registerAddress constByte
      VM.incrementPC
    IncrementByConst registerAddress incByte -> do
      -- no carry flag setting here
      VM.withRegistersAction $ Registers.modifyVRegister registerAddress (+ incByte)
      VM.incrementPC
    SetToRegister registerAddressDest registerAddressSrc -> do
      VM.withRegistersAction $ do
        registerValue <- Registers.readVRegister registerAddressSrc
        Registers.writeVRegister registerAddressDest registerValue
      VM.incrementPC
    OrRegisterInplace registerAddressDest registerAddressSrc -> do
      VM.withRegistersAction $ inplaceBinaryOperation registerAddressDest registerAddressSrc (.|.)
      VM.incrementPC
    AndRegisterInplace registerAddressDest registerAddressSrc -> do
      VM.withRegistersAction $ inplaceBinaryOperation registerAddressDest registerAddressSrc (.&.)
      VM.incrementPC
    XorRegisterInplace registerAddressDest registerAddressSrc -> do
      VM.withRegistersAction $ inplaceBinaryOperation registerAddressDest registerAddressSrc xor
      VM.incrementPC
    IncrementByRegister registerAddressDest registerAddressSrc -> do
      VM.withRegistersAction $ inplaceBinaryOperationWithFlag registerAddressDest registerAddressSrc (+) (\old _ new -> new < old)
      VM.incrementPC
    DecrementByRegister registerAddressDest registerAddressSrc -> do
      VM.withRegistersAction $ inplaceBinaryOperationWithFlag registerAddressDest registerAddressSrc (-) (\old _ new -> new > old)
      VM.incrementPC
    ShiftRight registerAddress -> do
      VM.withRegistersAction $ do
        registerValue <- Registers.readVRegister registerAddress
        let newRegisterValue = unsafeShiftR registerValue 1
            flagRegisterValue = registerValue .&. 0x01
        Registers.writeVRegister registerAddress newRegisterValue
        Registers.writeVRegister flagRegister flagRegisterValue
      VM.incrementPC
    DecrementByRegisterReverse registerAddressDest registerAddressSrc -> do
      VM.withRegistersAction $ inplaceBinaryOperationWithFlag registerAddressDest registerAddressSrc (flip (-)) (\_ src new -> new > src)
      VM.incrementPC
    ShiftLeft registerAddress -> do
      VM.withRegistersAction $ do
        registerValue <- Registers.readVRegister registerAddress
        let newRegisterValue = unsafeShiftL registerValue 1
            flagRegisterValue = unsafeShiftR (registerValue .&. 0x80) 7
        Registers.writeVRegister registerAddress newRegisterValue
        Registers.writeVRegister flagRegister flagRegisterValue
      VM.incrementPC
    SkipNextIfRegisterNotEqualToRegister registerAddress0 registerAddress1 -> do
      registersAreNotEqual <-
        VM.withRegistersAction $
          (/=) <$> Registers.readVRegister registerAddress0 <*> Registers.readVRegister registerAddress1
      VM.incrementPC
      when registersAreNotEqual VM.incrementPC
    SetAddressRegisterToConst memoryAddress -> do
      VM.withRegistersAction $ Registers.writeAddrRegister memoryAddress
      VM.incrementPC
    JumpToAddressWithOffset baseMemoryAddress -> do
      registerValue <- VM.withRegistersAction $ Registers.readVRegister v0Register
      case Finite.strengthenN $ Finite.add baseMemoryAddress (word8ToFinite registerValue :: Finite 255) of
        Nothing -> VM.throwVMError "attempted to jump beyond memory bounds"
        Just jumpAddress -> VM.setPC jumpAddress
    SetToRandomWithMask registerAddress mask -> do
      randomByte <- VM.randomByte
      VM.withRegistersAction $ Registers.writeVRegister registerAddress (randomByte .&. mask)
      VM.incrementPC
    DrawSpriteAtCoords registerAddressX registerAddressY spriteHeight -> do
      (basePosX, basePosY, baseSpriteAddress) <-
        VM.withRegistersAction $ do
          basePosX <- fmap word8ToFinite $ Registers.readVRegister registerAddressX
          basePosY <- fmap word8ToFinite $ Registers.readVRegister registerAddressY
          baseSpriteAddress <- Registers.readAddrRegister
          pure (basePosX, basePosY, baseSpriteAddress)
      anyFlippedPixels <- drawSprite basePosX basePosY baseSpriteAddress spriteHeight False
      VM.withRegistersAction $ Registers.writeVRegister flagRegister (toFlagValue anyFlippedPixels)
      VM.renderFrozenScreenBufferData
      VM.incrementPC
    SkipNextIfKeyPressed registerAddress -> do
      registerValue <- VM.withRegistersAction $ Registers.readVRegister registerAddress
      keyIsPressed <- checkWord8IsPressed registerValue
      when keyIsPressed VM.incrementPC
      VM.incrementPC
    SkipNextIfKeyNotPressed registerAddress -> do
      registerValue <- VM.withRegistersAction $ Registers.readVRegister registerAddress
      keyIsNotPressed <- fmap not $ checkWord8IsPressed registerValue
      when keyIsNotPressed VM.incrementPC
      VM.incrementPC
    SetToDelayTimerValue registerAddress -> do
      delayTimerValue <- VM.withTimersAction Timers.getDelayTimer
      VM.withRegistersAction $ Registers.writeVRegister registerAddress delayTimerValue
      VM.incrementPC
    SetToKeyboardKey registerAddress -> do
      keyboardWord <- fmap keyboardKeyToWord8 VM.getKeyboardKey
      VM.withRegistersAction $ Registers.writeVRegister registerAddress keyboardWord
      VM.incrementPC
    SetDelayTimerToRegister registerAddress -> do
      registerValue <- VM.withRegistersAction $ Registers.readVRegister registerAddress
      VM.withTimersAction $ Timers.setDelayTimer registerValue
      VM.incrementPC
    SetSoundTimerToRegister registerAddress -> do
      registerValue <- VM.withRegistersAction $ Registers.readVRegister registerAddress
      VM.withTimersAction $ Timers.setSoundTimer registerValue
      VM.incrementPC
    IncrementAddressRegisterByRegister _ -> unimplemented opCode
    GetLetterSpriteAddress _ -> unimplemented opCode
    StoreBinaryCodedDecimalRep _ -> unimplemented opCode
    DumpRegisters _ -> unimplemented opCode
    LoadRegisters _ -> unimplemented opCode

unimplemented :: OpCode -> VM.Action stackSize ()
unimplemented opCode = VM.throwVMError $ "unimplemented opCode: " <> show opCode

drawSprite :: ScreenX -> ScreenY -> MemoryAddress -> SpriteHeight -> Bool -> VM.Action stackSize Bool
drawSprite basePosX basePosY baseSpriteAddress spriteHeight anyFlippedPixels =
  case subOne spriteHeight of
    Nothing -> pure anyFlippedPixels
    Just nextSpriteHeight -> do
      rowData <- VM.withMemoryAction $ Memory.readMemory baseSpriteAddress
      anyFlippedPixelsInRow <- VM.withScreenBufferAction $ drawSpriteRow basePosX basePosY rowData False
      let anyFlippedPixels' = anyFlippedPixels || anyFlippedPixelsInRow
      case addOne basePosY of
        Nothing -> pure anyFlippedPixels'
        Just nextPosY ->
          case addOne baseSpriteAddress of
            Nothing ->
              if nextSpriteHeight == 0
                then pure anyFlippedPixels'
                else VM.throwVMError $ "attempted to read memory out of bounds when drawing row " <> show nextPosY <> " of sprite"
            Just nextSpriteAddress -> drawSprite basePosX nextPosY nextSpriteAddress nextSpriteHeight anyFlippedPixels'

drawSpriteRow :: ScreenX -> ScreenY -> Word8 -> Bool -> ScreenBuffer.Action Bool
drawSpriteRow rowStartX rowStartY rowData anyFlippedPixels =
  if rowData == 0x00
    then pure anyFlippedPixels
    else do
      pixelWasFlipped <-
        if rowData .&. 0x80 /= 0x00
          then ScreenBuffer.setPixelOn rowStartX rowStartY
          else pure False
      let anyFlippedPixels' = anyFlippedPixels || pixelWasFlipped
      case addOne rowStartX of
        Nothing -> pure anyFlippedPixels'
        Just nextRowX -> drawSpriteRow nextRowX rowStartY (unsafeShiftL rowData 1) anyFlippedPixels'

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
  Registers.writeVRegister flagRegister $
    toFlagValue (shouldSetFlag destRegisterValue srcRegisterValue newDestRegisterValue)

checkWord8IsPressed :: Word8 -> VM.Action stackSize Bool
checkWord8IsPressed registerValue =
  case word8ToKeyboardKey registerValue of
    Nothing -> pure False
    Just keyboardKey -> VM.isKeyPressed keyboardKey

toFlagValue :: Bool -> Word8
toFlagValue bool = if bool then 0x01 else 0x00

word8ToFinite :: TypeNats.KnownNat n => Word8 -> Finite n
word8ToFinite = Finite.finite . fromIntegral

keyboardKeyToWord8 :: KeyboardKey -> Word8
keyboardKeyToWord8 = fromIntegral . Finite.getFinite

word8ToKeyboardKey :: Word8 -> Maybe KeyboardKey
word8ToKeyboardKey = Finite.packFinite . fromIntegral

v0Register :: VRegisterAddress
v0Register = Finite.finite 0

flagRegister :: VRegisterAddress
flagRegister = Finite.finite 15
