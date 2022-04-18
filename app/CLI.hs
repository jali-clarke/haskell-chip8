module CLI (Options(..), parser) where

import Options.Applicative

data Options = Options {
  romFilePath :: FilePath,
  maxStackSize :: Int
  }

parser :: Parser Options
parser = Options <$> romFilePathParser <*> maxStackSizeParser

romFilePathParser :: Parser FilePath
romFilePathParser = strArgument $ metavar "ROM_FILE_PATH" <> help "path to rom file"

maxStackSizeParser :: Parser Int
maxStackSizeParser =
  option auto $
    metavar "MAX_STACK_SIZE" <> long "max-stack-size" <> showDefault <> value 50 <> help "max stack size / recursion depth"
