import qualified CLI
import Options.Applicative ((<**>))
import qualified Options.Applicative as Options

main :: IO ()
main =
  let parserWithInfo =
        Options.info (CLI.parser <**> Options.helper) $
          Options.fullDesc <> Options.progDesc "emulate execution of a CHIP-8 rom"
   in Options.execParser parserWithInfo >>= execCLIOpts

execCLIOpts :: CLI.Options -> IO ()
execCLIOpts options =
  putStrLn $
    "romFilePath: " <> CLI.romFilePath options <> " maxStackSize: " <> show (CLI.maxStackSize options)
