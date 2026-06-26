module DataTypes (applyCommand, GenericRecord (..), GenericTable (..), emptyTable) where

import Data.Vector (Vector, empty)
import Data.ByteString (ByteString)
import ParsingTypes (CommandData) 

newtype GenericRecord = Record (Vector ByteString) deriving (Show, Eq)
newtype GenericTable = Table (Vector GenericRecord) deriving (Show, Eq)

applyCommand :: GenericTable -> CommandData -> GenericTable
applyCommand = undefined

emptyTable :: GenericTable
emptyTable = Table empty