{-# LANGUAGE DataKinds #-}

module OpCode.Decode
  ( decode,
  )
where

import BaseTypes
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.))
import Data.Finite (Finite)
import Data.Word (Word8)
import OpCode.Type

decode :: OpCodeBin -> Maybe OpCode
decode opCodeBin =
  case opCodeBin .&. 0xF000 of
    0x0000 ->
      case opCodeBin of
        0x00E0 -> Just ClearDisplay
        0x00EE -> Just ReturnFromSubroutine
        _ -> Just $ decodeNNNOpCode MachineCodeCall opCodeBin
    0x1000 -> Just $ decodeNNNOpCode JumpToAddress opCodeBin
    0x2000 -> Just $ decodeNNNOpCode CallSubroutine opCodeBin
    0x3000 -> Just $ decodeXNNOpCode SkipNextIfRegisterEqualToConst opCodeBin
    0x4000 -> Just $ decodeXNNOpCode SkipNextIfRegisterNotEqualToConst opCodeBin
    0x5000 ->
      case opCodeBin .&. 0x000F of
        0x0000 -> Just $ decodeXYKOpCode SkipNextIfRegisterEqualToRegister opCodeBin
        _ -> Nothing
    0x6000 -> Just $ decodeXNNOpCode SetToConst opCodeBin
    0x7000 -> Just $ decodeXNNOpCode IncrementByConst opCodeBin
    0x8000 ->
      case opCodeBin .&. 0x000F of
        0x0000 -> Just $ decodeXYKOpCode SetToRegister opCodeBin
        0x0001 -> Just $ decodeXYKOpCode OrRegisterInplace opCodeBin
        0x0002 -> Just $ decodeXYKOpCode AndRegisterInplace opCodeBin
        0x0003 -> Just $ decodeXYKOpCode XorRegisterInplace opCodeBin
        0x0004 -> Just $ decodeXYKOpCode IncrementByRegister opCodeBin
        0x0005 -> Just $ decodeXYKOpCode DecrementByRegister opCodeBin
        0x0006 -> Just $ decodeXKKOpCode ShiftRight opCodeBin
        0x0007 -> Just $ decodeXYKOpCode DecrementByRegisterReverse opCodeBin
        0x000E -> Just $ decodeXKKOpCode ShiftLeft opCodeBin
        _ -> Nothing
    0x9000 ->
      case opCodeBin .&. 0x000F of
        0x0000 -> Just $ decodeXYKOpCode SkipNextIfRegisterNotEqualToRegister opCodeBin
        _ -> Nothing
    0xA000 -> Just $ decodeNNNOpCode SetAddressRegisterToConst opCodeBin
    0xB000 -> Just $ decodeNNNOpCode JumpToAddressWithOffset opCodeBin
    0xC000 -> Just $ decodeXNNOpCode SetToRandomWithMask opCodeBin
    0xD000 -> Just $ decodeXYNOpCode DrawSpriteAtCoords opCodeBin
    0xE000 ->
      case opCodeBin .&. 0x00FF of
        0x009E -> Just $ decodeXKKOpCode SkipNextIfKeyPressed opCodeBin
        0x00A1 -> Just $ decodeXKKOpCode SkipNextIfKeyNotPressed opCodeBin
        _ -> Nothing
    0xF000 ->
      case opCodeBin .&. 0x00FF of
        0x0007 -> Just $ decodeXKKOpCode SetToDelayTimerValue opCodeBin
        0x000A -> Just $ decodeXKKOpCode SetToKeyboardKey opCodeBin
        0x0015 -> Just $ decodeXKKOpCode SetDelayTimerToRegister opCodeBin
        0x0018 -> Just $ decodeXKKOpCode SetSoundTimerToRegister opCodeBin
        0x001E -> Just $ decodeXKKOpCode IncrementAddressRegisterByRegister opCodeBin
        0x0029 -> Just $ decodeXKKOpCode GetLetterSpriteAddress opCodeBin
        0x0033 -> Just $ decodeXKKOpCode StoreBinaryCodedDecimalRep opCodeBin
        0x0055 -> Just $ decodeXKKOpCode DumpRegisters opCodeBin
        0x0065 -> Just $ decodeXKKOpCode LoadRegisters opCodeBin
        _ -> Nothing
    _ -> Nothing

decodeNNNOpCode :: (Finite 4096 -> a) -> OpCodeBin -> a
decodeNNNOpCode toDecodedType opCodeBin = toDecodedType . fromIntegral $ opCodeBin .&. 0x0FFF

decodeXNNOpCode :: (Finite 16 -> Word8 -> a) -> OpCodeBin -> a
decodeXNNOpCode toDecodedType opCodeBin =
  let nybble = getNybble 8 opCodeBin
      byte = fromIntegral $ opCodeBin .&. 0x00FF
   in toDecodedType nybble byte

decodeXYKOpCode :: (Finite 16 -> Finite 16 -> a) -> OpCodeBin -> a
decodeXYKOpCode toDecodedType opCodeBin =
  let nybbleX = getNybble 8 opCodeBin
      nybbleY = getNybble 4 opCodeBin
   in toDecodedType nybbleX nybbleY

decodeXYNOpCode :: (Finite 16 -> Finite 16 -> Finite 16 -> a) -> OpCodeBin -> a
decodeXYNOpCode toDecodedType opCodeBin =
  let nybbleX = getNybble 8 opCodeBin
      nybbleY = getNybble 4 opCodeBin
      spriteHeight = getNybble 0 opCodeBin
   in toDecodedType nybbleX nybbleY spriteHeight

decodeXKKOpCode :: (Finite 16 -> a) -> OpCodeBin -> a
decodeXKKOpCode toDecodedType opCodeBin =
  let nybble = getNybble 8 opCodeBin
   in toDecodedType nybble

getNybble :: Finite 16 -> OpCodeBin -> Finite 16
getNybble offset opCodeBin =
  let shiftAmount = fromIntegral offset
      mask = unsafeShiftL 0x000F shiftAmount
   in fromIntegral $ unsafeShiftR (opCodeBin .&. mask) shiftAmount
