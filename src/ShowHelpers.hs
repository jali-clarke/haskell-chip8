module ShowHelpers
  ( rows,
    showMemoryAddress,
    showOpCodeBin,
    showRegisterAddress,
    showSpriteHeight,
    showWord8,
  )
where

import BaseTypes
import qualified Data.Finite as Finite
import Data.Word (Word8)
import qualified Numeric

showMemoryAddress :: MemoryAddress -> String
showMemoryAddress = showHexValue 3

showOpCodeBin :: OpCodeBin -> String
showOpCodeBin = showHexValue 4

showRegisterAddress :: VRegisterAddress -> String
showRegisterAddress = showHexValue 1 . Finite.getFinite

showSpriteHeight :: SpriteHeight -> String
showSpriteHeight = showHexValue 1 . Finite.getFinite

showWord8 :: Word8 -> String
showWord8 = showHexValue 2

rows :: Int -> [a] -> [[a]]
rows rowWidth elems =
  case elems of
    [] -> []
    _ -> let (row, rest) = splitAt rowWidth elems in row : rows rowWidth rest

showHexValue :: (Integral a, Show a) => Int -> a -> String
showHexValue padding n = "0x" <> leftPadZeroes padding (Numeric.showHex n "")

leftPadZeroes :: Int -> String -> String
leftPadZeroes minWidth str = take (minWidth - length str) (repeat '0') <> str
