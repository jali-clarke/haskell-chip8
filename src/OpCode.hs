{-# LANGUAGE DataKinds #-}

module OpCode
  ( OpCode (..),
    decode,
  )
where

import BaseTypes
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.))
import Data.Finite (Finite)
import qualified Data.Finite as Finite
import Data.Word (Word8)

-- see https://en.wikipedia.org/wiki/CHIP-8#Opcode_table
-- it's all copy-pasta'd from there

data OpCode
  = -- 0NNN
    MachineCodeCall MemoryAddress
  | -- 00E0
    ClearDisplay
  | -- 00EE
    ReturnFromSubroutine
  | -- 1NNN
    JumpToAddress MemoryAddress
  | -- 2NNN
    CallSubroutine MemoryAddress
  | -- 3XNN
    SkipNextIfRegisterEqualToConst VRegisterAddress Word8
  | -- 4XNN
    SkipNextIfRegisterNotEqualToConst VRegisterAddress Word8
  | -- 5XY0
    SkipNextIfRegisterEqualToRegister VRegisterAddress VRegisterAddress
  | -- 6XNN
    SetToConst VRegisterAddress Word8
  | -- 7XNN
    -- will not* set carry flag VF regardless of overflow
    IncrementByConst VRegisterAddress Word8
  | -- 8XY0
    -- Vx = Vy
    SetToRegister VRegisterAddress VRegisterAddress
  | -- 8XY1
    -- Vx |= Vy
    OrRegisterInplace VRegisterAddress VRegisterAddress
  | -- 8XY2
    -- Vx &= Vy
    AndRegisterInplace VRegisterAddress VRegisterAddress
  | -- 8XY3
    -- Vx ^= Vy
    XorRegisterInplace VRegisterAddress VRegisterAddress
  | -- 8XY4
    -- Vx += Vy; *will* set carry flag VF to 1 if overflow, 0 otherwise
    IncrementByRegister VRegisterAddress VRegisterAddress
  | -- 8XY5
    -- Vx -= Vy; *will* set borrow flag VF to 1 if underflow, 0 otherwise
    DecrementByRegister VRegisterAddress VRegisterAddress
  | -- 8XY6
    -- Vx >>= 1; y is ignored; stores LSB in VF
    ShiftRight VRegisterAddress
  | -- 8XY7
    -- Vx = Vy - Vx; *will* set borrow flag VF to 1 if underflow, 0 otherwise
    DecrementByRegisterReverse VRegisterAddress VRegisterAddress
  | -- 8XYE
    -- Vx <<= 1; y is ignored; stores MSB in VF
    ShiftLeft VRegisterAddress
  | -- 9XY0
    SkipNextIfRegisterNotEqualToRegister VRegisterAddress VRegisterAddress
  | -- ANNN
    SetAddressRegisterToConst MemoryAddress
  | -- BNNN
    -- jump to V0 + NNN
    JumpToAddressWithOffset MemoryAddress
  | -- CXNN
    -- Vx = random(0, 255) & NN
    SetToRandomWithMask VRegisterAddress Word8
  | -- DXYN
    -- draw sprite at coords x = Vx, y = Vy; width is 8px, height is Npx; data is read starting from the address register
    -- will also set VF if there's a sprite collision, i.e. if a pixel got flipped from on to off
    DrawSpriteAtCoords VRegisterAddress VRegisterAddress SpriteHeight
  | -- EX9E
    SkipNextIfKeyPressed VRegisterAddress KeyboardKey
  | -- EXA1
    SkipNextIfKeyNotPressed VRegisterAddress KeyboardKey
  | -- FX07
    SetToDelayTimerValue VRegisterAddress
  | -- FX0A
    -- stores value equal to key pressed in Vx
    -- will block until a key between 0 and F is pressed
    SetToKeyboardKey VRegisterAddress
  | -- FX15
    SetDelayTimerToRegister VRegisterAddress
  | -- FX18
    SetSoundTimerToRegister VRegisterAddress
  | -- FX1E
    -- will not* set carry flag VF regardless of overflow
    IncrementAddressRegisterByRegister VRegisterAddress
  | -- FX29
    -- sets the address register to the location of the sprite for the character in VX
    -- characters 0-F (in hex) are represented by a 4x5 font
    GetSpriteLetterAddress VRegisterAddress
  | -- FX33
    -- take the decimal representation of VX and:
    -- place the hundreds digit in memory at the location specified by the address register
    -- the tens digit at the location specified by the address register + 1
    -- and the ones digit at the location specified by the address register + 2
    StoreBinaryCodedDecimalRep VRegisterAddress
  | -- FX55
    -- stores from V0 to Vx (including Vx) in memory, starting at the address specified by the address register
    DumpRegisters VRegisterAddress
  | -- FX65
    -- loads from V0 to Vx (including Vx) in memory, starting at the address specified by the address register
    LoadRegisters VRegisterAddress

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
        0x0006 -> undefined
        0x0007 -> Just $ decodeXYKOpCode DecrementByRegisterReverse opCodeBin
        0x000E -> undefined
        _ -> Nothing
    0x9000 ->
      case opCodeBin .&. 0x000F of
        0x0000 -> Just $ decodeXYKOpCode SkipNextIfRegisterNotEqualToRegister opCodeBin
        _ -> Nothing
    0xA000 -> Just $ decodeNNNOpCode SetAddressRegisterToConst opCodeBin
    0xB000 -> Just $ decodeNNNOpCode JumpToAddressWithOffset opCodeBin
    0xC000 -> Just $ decodeXNNOpCode SetToRandomWithMask opCodeBin
    0xD000 -> Just $ decodeXYNOpCode DrawSpriteAtCoords opCodeBin
    0xE000 -> undefined
    0xF000 -> undefined
    _ -> Nothing

decodeNNNOpCode :: (MemoryAddress -> a) -> OpCodeBin -> a
decodeNNNOpCode toDecodedType opCodeBin = toDecodedType . Finite.finite . fromIntegral $ opCodeBin .&. 0x0FFF

decodeXNNOpCode :: (VRegisterAddress -> Word8 -> a) -> OpCodeBin -> a
decodeXNNOpCode toDecodedType opCodeBin =
  let targetRegister = getNybble (Finite.finite 8) opCodeBin
      byte = fromIntegral $ opCodeBin .&. 0x00FF
   in toDecodedType targetRegister byte

decodeXYKOpCode :: (VRegisterAddress -> VRegisterAddress -> a) -> OpCodeBin -> a
decodeXYKOpCode toDecodedType opCodeBin =
  let targetRegisterX = getNybble (Finite.finite 8) opCodeBin
      targetRegisterY = getNybble (Finite.finite 4) opCodeBin
   in toDecodedType targetRegisterX targetRegisterY

decodeXYNOpCode :: (VRegisterAddress -> VRegisterAddress -> SpriteHeight -> a) -> OpCodeBin -> a
decodeXYNOpCode toDecodedType opCodeBin =
  let targetRegisterX = getNybble (Finite.finite 8) opCodeBin
      targetRegisterY = getNybble (Finite.finite 4) opCodeBin
      spriteHeight = getNybble (Finite.finite 0) opCodeBin
   in toDecodedType targetRegisterX targetRegisterY spriteHeight

getNybble :: Finite 16 -> OpCodeBin -> Finite 16
getNybble offset opCodeBin =
  let shiftAmount = fromIntegral (Finite.getFinite offset)
      mask = unsafeShiftL 0x000F shiftAmount
   in Finite.finite . fromIntegral $ unsafeShiftR (opCodeBin .&. mask) shiftAmount
