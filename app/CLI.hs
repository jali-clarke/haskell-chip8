module CLI (Options (..), parser) where

import Options.Applicative

-- ordering determines order of appearance in help text
data Options = Options
  { romFilePath :: FilePath,
    maxStackSize :: Int,
    tickRate :: Int, -- microseconds
    verboseMode :: Bool
  }

parser :: Parser Options
parser = Options <$> romFilePathParser <*> maxStackSizeParser <*> tickRateParser <*> verboseModeParser

maxStackSizeParser :: Parser Int
maxStackSizeParser =
  option auto $
    metavar "MAX_STACK_SIZE" <> long "max-stack-size" <> short 's' <> showDefault <> value 50 <> help "max stack size / recursion depth"

romFilePathParser :: Parser FilePath
romFilePathParser = strArgument $ metavar "ROM_FILE_PATH" <> help "path to rom file"

tickRateParser :: Parser Int
tickRateParser =
  option auto $
    metavar "TICK_RATE_INTERVAL" <> long "tick-rate" <> short 't' <> showDefault <> value 2000 <> help "tick rate in microseconds per instruction"

verboseModeParser :: Parser Bool
verboseModeParser = switch $ long "verbose" <> short 'v' <> help "enable debug logging if flag is set"
