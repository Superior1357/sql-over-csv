module CommandExceptions where

import Control.Exception (Exception)
import Data.ByteString (ByteString)

type ErrorMessage = ByteString

data CommandException = ColumnNotFoundException ErrorMessage |
                        ColumnNameDuplicatedException ErrorMessage |
                        UnableToInterpretException ErrorMessage |
                        InvalidArgCountException ErrorMessage | -- TODO: this is not very descriptive
                        HeadersDifferException ErrorMessage |
                        InvalidTableFormatException ErrorMessage deriving (Show, Eq)

instance Exception CommandException
    