{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import BaseTypes
import qualified CLI
import Control.Monad (forever)
import qualified Data.ByteString as ByteString
import Data.Foldable (traverse_)
import qualified Data.Vector.Unboxed.Sized as SizedVector
import qualified GHC.TypeNats as TypeNats
import qualified OpCode
import Options.Applicative ((<**>))
import qualified Options.Applicative as Options
import qualified System.Console.ANSI as ANSI
import qualified VM
import VM.MachineCallbacks (MachineCallbacks (..))

main :: IO ()
main =
  let parserWithInfo =
        Options.info (CLI.parser <**> Options.helper) $
          Options.fullDesc <> Options.progDesc "emulate execution of a CHIP-8 rom"
   in Options.execParser parserWithInfo >>= execCLIOpts

execCLIOpts :: CLI.Options -> IO ()
execCLIOpts options =
  let callbacks =
        MachineCallbacks
          { blockingGetKeyboardKey = pure 'f',
            isKeyPressed = \_ -> pure False,
            randomByte = pure 0,
            renderFrozenScreenBufferData = renderToTerminal
          }
   in do
        romBytes <- ByteString.readFile (CLI.romFilePath options)
        let maxStackSize = CLI.maxStackSize options
        VM.withNewVMState callbacks maxStackSize romBytes $ \maybeVmState ->
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
    case OpCode.decode opCodeBin of
      Nothing -> VM.throwVMError $ "unknown opcode: " <> show opCodeBin
      Just opCode -> OpCode.exec opCode

renderToTerminal :: SizedVector.Vector ScreenBufferSize Bool -> IO ()
renderToTerminal screenData = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
  traverse_ printRow (rows screenData)

rows :: SizedVector.Vector ScreenBufferSize Bool -> [[Bool]]
rows = chunk 64 . SizedVector.toList

printRow :: [Bool] -> IO ()
printRow rowData = do
  traverse_ (\b -> if b then putChar '#' else putChar ' ') rowData
  putChar '\n'

chunk :: Int -> [a] -> [[a]]
chunk n as =
  case splitAt n as of
    ([], []) -> []
    (prefix, rest) -> prefix : chunk n rest
