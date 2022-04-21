module OpCode.Type
  ( OpCode (..),
  )
where

import BaseTypes
import Data.Word (Word8)
import qualified ShowHelpers

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
    SkipNextIfKeyPressed VRegisterAddress
  | -- EXA1
    SkipNextIfKeyNotPressed VRegisterAddress
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
    GetLetterSpriteAddress VRegisterAddress
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

instance Show OpCode where
  show opCode =
    case opCode of
      MachineCodeCall memoryAddress ->
        showMemoryAddressOpCode "MachineCodeCall" memoryAddress
      ClearDisplay ->
        "ClearDisplay"
      ReturnFromSubroutine ->
        "ReturnFromSubroutine"
      JumpToAddress memoryAddress ->
        showMemoryAddressOpCode "JumpToAddress" memoryAddress
      CallSubroutine memoryAddress ->
        showMemoryAddressOpCode "CallSubroutine" memoryAddress
      SkipNextIfRegisterEqualToConst registerAddress byte ->
        showRegisterAddressByteOpCode "SkipNextIfRegisterEqualToConst" registerAddress byte
      SkipNextIfRegisterNotEqualToConst registerAddress byte ->
        showRegisterAddressByteOpCode "SkipNextIfRegisterNotEqualToConst" registerAddress byte
      SkipNextIfRegisterEqualToRegister registerAddress0 registerAddress1 ->
        showRegisterAddressRegisterAddressOpCode "SkipNextIfRegisterEqualToRegister" registerAddress0 registerAddress1
      SetToConst registerAddress byte ->
        showRegisterAddressByteOpCode "SetToConst" registerAddress byte
      IncrementByConst registerAddress byte ->
        showRegisterAddressByteOpCode "IncrementByConst" registerAddress byte
      SetToRegister registerAddressDest registerAddressSrc ->
        showRegisterAddressRegisterAddressOpCode "SetToRegister" registerAddressDest registerAddressSrc
      OrRegisterInplace registerAddressDest registerAddressSrc ->
        showRegisterAddressRegisterAddressOpCode "OrRegisterInplace" registerAddressDest registerAddressSrc
      AndRegisterInplace registerAddressDest registerAddressSrc ->
        showRegisterAddressRegisterAddressOpCode "AndRegisterInplace" registerAddressDest registerAddressSrc
      XorRegisterInplace registerAddressDest registerAddressSrc ->
        showRegisterAddressRegisterAddressOpCode "XorRegisterInplace" registerAddressDest registerAddressSrc
      IncrementByRegister registerAddressDest registerAddressSrc ->
        showRegisterAddressRegisterAddressOpCode "IncrementByRegister" registerAddressDest registerAddressSrc
      DecrementByRegister registerAddressDest registerAddressSrc ->
        showRegisterAddressRegisterAddressOpCode "DecrementByRegister" registerAddressDest registerAddressSrc
      ShiftRight registerAddressDest ->
        showRegisterAddressOpCode "ShiftRight" registerAddressDest
      DecrementByRegisterReverse registerAddressDest registerAddressSrc ->
        showRegisterAddressRegisterAddressOpCode "DecrementByRegisterReverse" registerAddressDest registerAddressSrc
      ShiftLeft registerAddressDest ->
        showRegisterAddressOpCode "ShiftLeft" registerAddressDest
      SkipNextIfRegisterNotEqualToRegister registerAddressDest registerAddressSrc ->
        showRegisterAddressRegisterAddressOpCode "SkipNextIfRegisterNotEqualToRegister" registerAddressDest registerAddressSrc
      SetAddressRegisterToConst memoryAddress ->
        showMemoryAddressOpCode "SetAddressRegisterToConst" memoryAddress
      JumpToAddressWithOffset memoryAddress ->
        showMemoryAddressOpCode "JumpToAddressWithOffset" memoryAddress
      SetToRandomWithMask registerAddress byte ->
        showRegisterAddressByteOpCode "SetToRandomWithMask" registerAddress byte
      DrawSpriteAtCoords registerAddressX registerAddressY spriteHeight ->
        showDrawOpCode "DrawSpriteAtCoords" registerAddressX registerAddressY spriteHeight
      SkipNextIfKeyPressed registerAddress ->
        showRegisterAddressOpCode "SkipNextIfKeyPressed" registerAddress
      SkipNextIfKeyNotPressed registerAddress ->
        showRegisterAddressOpCode "SkipNextIfKeyNotPressed" registerAddress
      SetToDelayTimerValue registerAddress ->
        showRegisterAddressOpCode "SetToDelayTimerValue" registerAddress
      SetToKeyboardKey registerAddress ->
        showRegisterAddressOpCode "SetToKeyboardKey" registerAddress
      SetDelayTimerToRegister registerAddress ->
        showRegisterAddressOpCode "SetDelayTimerToRegister" registerAddress
      SetSoundTimerToRegister registerAddress ->
        showRegisterAddressOpCode "SetSoundTimerToRegister" registerAddress
      IncrementAddressRegisterByRegister registerAddress ->
        showRegisterAddressOpCode "IncrementAddressRegisterByRegister" registerAddress
      GetLetterSpriteAddress registerAddress ->
        showRegisterAddressOpCode "GetLetterSpriteAddress" registerAddress
      StoreBinaryCodedDecimalRep registerAddress ->
        showRegisterAddressOpCode "StoreBinaryCodedDecimalRep" registerAddress
      DumpRegisters registerAddress ->
        showRegisterAddressOpCode "DumpRegisters" registerAddress
      LoadRegisters registerAddress ->
        showRegisterAddressOpCode "LoadRegisters" registerAddress

showMemoryAddressOpCode :: String -> MemoryAddress -> String
showMemoryAddressOpCode label memoryAddress = label <> " " <> ShowHelpers.showMemoryAddress memoryAddress

showRegisterAddressByteOpCode :: String -> VRegisterAddress -> Word8 -> String
showRegisterAddressByteOpCode label registerAddress byte =
  label <> " " <> ShowHelpers.showRegisterAddress registerAddress <> " " <> ShowHelpers.showWord8 byte

showRegisterAddressRegisterAddressOpCode :: String -> VRegisterAddress -> VRegisterAddress -> String
showRegisterAddressRegisterAddressOpCode label registerAddressDest registerAddressSrc =
  label <> " " <> ShowHelpers.showRegisterAddress registerAddressDest <> " " <> ShowHelpers.showRegisterAddress registerAddressSrc

showRegisterAddressOpCode :: String -> VRegisterAddress -> String
showRegisterAddressOpCode label registerAddress = label <> " " <> ShowHelpers.showRegisterAddress registerAddress

showDrawOpCode :: String -> VRegisterAddress -> VRegisterAddress -> SpriteHeight -> String
showDrawOpCode label registerAddressX registerAddressY spriteHeight =
  label <> " " <> ShowHelpers.showRegisterAddress registerAddressX <> " " <> ShowHelpers.showRegisterAddress registerAddressY <> " " <> ShowHelpers.showSpriteHeight spriteHeight
