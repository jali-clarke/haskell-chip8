{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import qualified CLI
import Control.Monad (forever)
import qualified Data.ByteString as ByteString
import Options.Applicative ((<**>))
import qualified Options.Applicative as Options
import qualified OpCode
import qualified VM
import VM.MachineCallbacks (MachineCallbacks(..))
import qualified GHC.TypeNats as TypeNats

main :: IO ()
main =
  let parserWithInfo =
        Options.info (CLI.parser <**> Options.helper) $
          Options.fullDesc <> Options.progDesc "emulate execution of a CHIP-8 rom"
   in Options.execParser parserWithInfo >>= execCLIOpts

execCLIOpts :: CLI.Options -> IO ()
execCLIOpts options =
  let callbacks = MachineCallbacks {
          blockingGetKeyboardKey = pure 'f',
          isKeyPressed = \_ -> pure False,
          randomByte = pure 0,
          renderFrozenScreenBufferData = \_ -> pure ()
        }
   in do
    romBytes <- ByteString.readFile (CLI.romFilePath options)
    let maxStackSize = CLI.maxStackSize options
    VM.withNewVMState callbacks maxStackSize romBytes $ \maybeVmState ->
      case maybeVmState of
        Left err -> putStrLn err
        Right vmState -> do
          (maybeResult, _) <- VM.runAction vmLoop vmState
          case maybeResult of
            Left err -> putStrLn err
            Right () -> pure ()

vmLoop :: TypeNats.KnownNat stackSize => VM.Action stackSize ()
vmLoop =
  forever $ do
    opCodeBin <- VM.getOpCodeBin
    case OpCode.decode opCodeBin of
      Nothing -> VM.throwVMError $ "unknown opcode: " <> show opCodeBin
      Just opCode -> OpCode.exec opCode
