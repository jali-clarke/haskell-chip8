module CLI (Options (..), parser) where

import Options.Applicative

data Options = Options
  { romFilePath :: FilePath,
    maxStackSize :: Int,
    verboseMode :: Bool
  }

parser :: Parser Options
parser = Options <$> romFilePathParser <*> maxStackSizeParser <*> verboseModeParser

romFilePathParser :: Parser FilePath
romFilePathParser = strArgument $ metavar "ROM_FILE_PATH" <> help "path to rom file"

maxStackSizeParser :: Parser Int
maxStackSizeParser =
  option auto $
    metavar "MAX_STACK_SIZE" <> long "max-stack-size" <> short 's' <> showDefault <> value 50 <> help "max stack size / recursion depth"

verboseModeParser :: Parser Bool
verboseModeParser = switch $ long "verbose" <> short 'v' <> help "enable debug logging if provided"
