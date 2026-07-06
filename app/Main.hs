{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where
import LibControl (runCommand, translateException)
import LibExceptions ( ApplicationException )

import System.Console.CmdArgs.Implicit
import Control.Monad (forever)
import System.IO (hFlush, stdout)
import Control.Exception (try)

data Args = NonInteractive { command :: String } |
            Interactive deriving (Show, Data, Typeable)

nonInteractive :: Args
nonInteractive = NonInteractive { command = def &= help "Command to execute" &= typ "COMMAND" }

interactive :: Args
interactive = Interactive &= auto

runSafe :: String -> IO ()
runSafe s = do
  result <- try $ LibControl.runCommand s :: IO (Either ApplicationException ())
  case result of
    Right r -> pure r
    Left exc -> putStrLn $ translateException exc

interactiveSession :: IO ()
interactiveSession = forever $ do
  putStr "> "
  hFlush stdout
  input <- getLine
  runSafe input

main :: IO ()
main = do
  arguments <- cmdArgs (modes [nonInteractive, interactive])
  case arguments of
    NonInteractive { command = cmd } -> runSafe cmd
    Interactive -> interactiveSession