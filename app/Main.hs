import qualified CLI
import Control.Monad (forever)
import qualified Data.ByteString as ByteString
import qualified GHC.TypeNats as TypeNats
import qualified OpCode
import Options.Applicative ((<**>))
import qualified Options.Applicative as Options
import Platform.SDL (withPlatform)
import qualified ShowHelpers
import qualified VM
import qualified VM.Config
import qualified VM.Config as VM (Config (Config))
import VM.Platform (Platform)

main :: IO ()
main =
  let parserWithInfo =
        Options.info (CLI.parser <**> Options.helper) $
          Options.fullDesc <> Options.progDesc "emulate execution of a CHIP-8 rom"
   in Options.execParser parserWithInfo >>= execCLIOpts

execCLIOpts :: CLI.Options -> IO ()
execCLIOpts options =
  withPlatform $ \platform -> do
    vmConfig <- toVMConfig options platform
    VM.withNewVMState vmConfig $ \maybeVmState ->
      case maybeVmState of
        Left err -> putStrLn err
        Right vmState -> do
          (maybeResult, endState) <- VM.runAction vmLoop vmState
          case maybeResult of
            Left err -> putStrLn $ err <> "\n"
            Right () -> pure ()
          VM.dumpState endState

vmLoop :: TypeNats.KnownNat stackSize => VM.Action stackSize ()
vmLoop =
  forever $ do
    opCodeBin <- VM.getOpCodeBin
    VM.debugLog $ "got raw opcode: " <> ShowHelpers.showOpCodeBin opCodeBin
    case OpCode.decode opCodeBin of
      Nothing -> VM.throwVMError $ "unknown opcode: " <> show opCodeBin
      Just opCode -> do
        VM.debugLog $ "executing parsed opcode: " <> show opCode
        OpCode.exec opCode

toVMConfig :: CLI.Options -> Platform -> IO VM.Config
toVMConfig options platform = do
  romBytes <- ByteString.readFile (CLI.romFilePath options)
  pure $
    VM.Config
      { VM.Config.platform = platform,
        VM.Config.maxStackSize = CLI.maxStackSize options,
        VM.Config.programRom = romBytes,
        VM.Config.shouldLog = CLI.verboseMode options
      }
