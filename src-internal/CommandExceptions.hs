module CommandExceptions where

import Control.Exception (Exception)
import Data.ByteString (ByteString)

type ErrorMessage = ByteString

-- | Thrown specifically by commands.
data CommandException = ColumnNotFoundException ErrorMessage |
                        ColumnNameDuplicatedException ErrorMessage |
                        UnableToInterpretException ErrorMessage |
                        InvalidArgCountException ErrorMessage |
                        HeadersDifferException ErrorMessage |
                        InvalidTableFormatException ErrorMessage deriving (Show, Eq)

instance Exception CommandException
    