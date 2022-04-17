{-# LANGUAGE DataKinds #-}

module BaseTypes where

import Data.Finite (Finite)

type MemorySize = 4096

type NumRegisters = 16

type MaxSpriteHeight = 4

type NumKeyboardKeys = 16

type MemoryAddress = Finite MemorySize

type VRegisterAddress = Finite NumRegisters

type SpriteHeight = Finite MaxSpriteHeight

type KeyboardKey = Finite NumKeyboardKeys
