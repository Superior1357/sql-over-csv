module DataTypes where

import Data.Vector (Vector)
import Data.ByteString (ByteString)

newtype GenericRecord = Record (Vector ByteString) deriving (Show, Eq)
newtype GenericTable = Table (Vector GenericRecord) deriving (Show, Eq)