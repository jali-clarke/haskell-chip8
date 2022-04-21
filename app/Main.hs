import qualified CLI
import Callbacks.SDL (withCallbacks)
import Control.Monad (forever)
import qualified Data.ByteString as ByteString
import qualified GHC.TypeNats as TypeNats
import qualified OpCode
import Options.Applicative ((<**>))
import qualified Options.Applicative as Options
import qualified ShowHelpers
import qualified VM
import qualified VM.Config
import qualified VM.Config as VM (Config (Config))
import VM.MachineCallbacks (MachineCallbacks)

main :: IO ()
main =
  let parserWithInfo =
        Options.info (CLI.parser <**> Options.helper) $
          Options.fullDesc <> Options.progDesc "emulate execution of a CHIP-8 rom"
   in Options.execParser parserWithInfo >>= execCLIOpts

execCLIOpts :: CLI.Options -> IO ()
execCLIOpts options =
  withCallbacks $ \callbacks -> do
    vmConfig <- toVMConfig options callbacks
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

toVMConfig :: CLI.Options -> MachineCallbacks -> IO VM.Config
toVMConfig options callbacks = do
  romBytes <- ByteString.readFile (CLI.romFilePath options)
  pure $
    VM.Config
      { VM.Config.machineCallbacks = callbacks,
        VM.Config.maxStackSize = CLI.maxStackSize options,
        VM.Config.programRom = romBytes,
        VM.Config.shouldLog = CLI.verboseMode options
      }
