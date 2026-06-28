{-#LANGUAGE FlexibleContexts#-}
module Commands where

import Data.List (intercalate)
import DataTypes (GenericTable(..), GenericRecord (Record))
import ParsingTypes (CommandData (..), Column, RecordValue)

import Data.Vector (fromList, singleton, empty, toList)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)

toByteStrings :: [String] -> [ByteString]
toByteStrings = map (TE.encodeUtf8 . T.pack)

create :: [Column] -> GenericTable
create colNames = Table $ singleton (Record $ fromList $ toByteStrings colNames)

insert :: GenericTable -> [Column] -> [[RecordValue]] -> GenericTable
insert (Table recordsV) cols values = let colsT = toByteStrings cols; prev@((Record header):records) = (toList recordsV); insertedIn = toList $ V.map (`elem` colsT) header in
        Table $ fromList (prev ++ next insertedIn)
    where
        next addVal = map (makeRecord addVal) values
        makeRecord addVal recordV = Record $ fromList $ toByteStrings $ fillAllZip addVal recordV

        fillAllZip (f:fZip) l = if f then let (li:lis) = l in (li:fillAllZip fZip lis)
                              else "":fillAllZip fZip l
        fillAllZip [] [] = []
        fillAllZip _ _ = undefined

applyCommand :: GenericTable -> CommandData -> GenericTable
applyCommand _ (Create colNames) = create colNames
applyCommand table (Insert colNames rs) = insert table colNames rs
applyCommand _ _ = undefined

emptyTable :: GenericTable
emptyTable = Table empty