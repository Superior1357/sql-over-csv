module Types where

newtype Command = Cmd (IO ())

class Stream a where
    write :: a -> String -> IO ()
    read :: a -> IO String