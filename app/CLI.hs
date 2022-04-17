module CLI (Options(..), parser) where

import Options.Applicative

data Options = Options {
  romFilePath :: FilePath
  }

parser :: Parser Options
parser = Options <$> filePathParser

filePathParser :: Parser FilePath
filePathParser = strOption $ metavar "ROM_FILE_PATH" <> help "path to rom file"
