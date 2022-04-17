{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module OpCode.Exec
  ( exec,
  )
where

import Control.Monad (when)
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
    _ -> unimplemented

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
