{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where
import qualified LibControl (runCommand)
import System.Console.CmdArgs.Implicit
import Control.Monad (forever)
import System.IO (hFlush, stdout)

data Args = NonInteractive { command :: String } |
            Interactive deriving (Show, Data, Typeable)

nonInteractive :: Args
nonInteractive = NonInteractive { command = def &= help "Command to execute" &= typ "COMMAND" }

interactive :: Args
interactive = Interactive &= auto

interactiveSession :: IO ()
interactiveSession = forever $ do
  putStr "> "
  hFlush stdout
  input <- getLine
  LibControl.runCommand input

main :: IO ()
main = do
  arguments <- cmdArgs (modes [nonInteractive, interactive])
  case arguments of
    NonInteractive { command = cmd } -> LibControl.runCommand cmd
    Interactive -> interactiveSession