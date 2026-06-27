{-#LANGUAGE FlexibleContexts#-}
{-# LANGUAGE RecordWildCards #-}
module Commands where

import Data.List (intercalate)
import DataTypes (GenericTable(..), GenericRecord (Record))
import ParsingTypes (CommandData (..), Column)
import Data.Vector (fromList, singleton, empty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

create :: [Column] -> GenericTable
create columnNames = Table $ singleton (Record $ fromList $ map (TE.encodeUtf8 . T.pack) columnNames)

applyCommand :: GenericTable -> CommandData -> GenericTable
applyCommand _ (Create columnNames) = create columnNames

emptyTable :: GenericTable
emptyTable = Table empty