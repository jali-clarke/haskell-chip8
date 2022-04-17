{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BaseTypes where

import Data.Finite (Finite)
import Data.Word (Word16)

type MemorySize = 4096

type NumRegisters = 16

type MaxSpriteHeight = 16

type NumKeyboardKeys = 16

type ScreenWidth = 64

type ScreenHeight = 32

type OpCodeBin = Word16

type MemoryAddress = Finite MemorySize

type VRegisterAddress = Finite NumRegisters

type StackAddress stackSize = Finite stackSize

type SpriteHeight = Finite MaxSpriteHeight

type KeyboardKey = Finite NumKeyboardKeys

type ScreenX = Finite ScreenWidth

type ScreenY = Finite ScreenHeight
