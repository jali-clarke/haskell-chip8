{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module BaseTypes where

import Data.Finite (Finite)
import Data.Word (Word16)
import GHC.TypeNats (type (*))

type MemorySize = 4096

type NumRegisters = 16

type MaxSpriteHeight = 16

type MaxSpriteWidth = 8

type NumKeyboardKeys = 16

type ScreenWidth = 64

type ScreenHeight = 32

type ScreenBufferSize = ScreenWidth * ScreenHeight

type OpCodeBin = Word16

type MemoryAddress = Finite MemorySize

type VRegisterAddress = Finite NumRegisters

type StackAddress stackSize = Finite stackSize

type SpriteHeight = Finite MaxSpriteHeight

type SpriteWidth = Finite MaxSpriteWidth

type KeyboardKey = Finite NumKeyboardKeys

type ScreenX = Finite ScreenWidth

type ScreenY = Finite ScreenHeight

type ScreenBufferAddress = Finite ScreenBufferSize
