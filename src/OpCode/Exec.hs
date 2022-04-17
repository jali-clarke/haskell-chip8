{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module OpCode.Exec
  ( exec,
  )
where

import OpCode.Type
import qualified VMState
import qualified VMState.ScreenBuffer as ScreenBuffer
import qualified VMState.Stack as Stack
import GHC.TypeNats (type (+), type (<=))
import qualified GHC.TypeNats as TypeNats

exec :: (TypeNats.KnownNat stackSize, stackSize <= stackSize + 2) => OpCode -> VMState.Action stackSize ()
exec opCode =
  case opCode of
    MachineCodeCall _ -> unimplemented
    ClearDisplay -> VMState.withScreenBufferAction ScreenBuffer.clearBuffer *> VMState.incrementPC
    ReturnFromSubroutine -> do
      returnAddress <- VMState.withStackAction Stack.popStack
      VMState.setPC returnAddress
      VMState.incrementPC
    JumpToAddress memoryAddress -> VMState.setPC memoryAddress
    CallSubroutine memoryAddress -> do
      currentPC <- VMState.getPC
      VMState.withStackAction $ Stack.pushStack currentPC
      VMState.setPC memoryAddress
    _ -> unimplemented

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
