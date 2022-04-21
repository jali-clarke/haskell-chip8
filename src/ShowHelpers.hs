module ShowHelpers
  ( rows,
    showMemoryAddress,
    showOpCodeBin,
    showWord8,
  )
where

import BaseTypes
import Data.Word (Word8)
import qualified Numeric

showMemoryAddress :: MemoryAddress -> String
showMemoryAddress = showHexValue 3

showOpCodeBin :: OpCodeBin -> String
showOpCodeBin = showHexValue 4

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
