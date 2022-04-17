module OpCode.Exec (
  exec
) where

import OpCode.Type
import qualified VMState
import qualified VMState.ScreenBuffer as ScreenBuffer

exec :: OpCode -> VMState.Action stackSize ()
exec opCode =
  case opCode of
    MachineCodeCall _ -> unimplemented
    ClearDisplay -> VMState.withScreenBufferAction ScreenBuffer.clearBuffer
    _ -> unimplemented
    -- ReturnFromSubroutine
    -- JumpToAddress MemoryAddress
    -- CallSubroutine MemoryAddress
    -- SkipNextIfRegisterEqualToConst VRegisterAddress Word8
    -- SkipNextIfRegisterNotEqualToConst VRegisterAddress Word8
    -- SkipNextIfRegisterEqualToRegister VRegisterAddress VRegisterAddress
    -- SetToConst VRegisterAddress Word8
    -- IncrementByConst VRegisterAddress Word8
    -- SetToRegister VRegisterAddress VRegisterAddress
    -- OrRegisterInplace VRegisterAddress VRegisterAddress
    -- AndRegisterInplace VRegisterAddress VRegisterAddress
    -- XorRegisterInplace VRegisterAddress VRegisterAddress
    -- IncrementByRegister VRegisterAddress VRegisterAddress
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
