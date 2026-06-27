module DataTypes (GenericRecord (..), GenericTable (..)) where

import Data.Vector (Vector, empty)
import Data.ByteString (ByteString)
import ParsingTypes (CommandData (..)) 
import Data.Csv (ToRecord)

newtype GenericRecord = Record (Vector ByteString) deriving (Show, Eq)
newtype GenericTable = Table (Vector GenericRecord) deriving (Show, Eq)