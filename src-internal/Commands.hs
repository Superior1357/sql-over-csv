{-#LANGUAGE FlexibleContexts#-}
module Commands where

import Data.List (intercalate)
import DataTypes (Record (..), Table (..), WhereCondition (..), CommandData(..))

import Data.Vector (fromList, singleton, empty, toList)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)

type ColumnType = ByteString
type RecordValueType = ByteString
type RecordType = Record (V.Vector RecordValueType)

type CommandDataType = CommandData ColumnType RecordValueType
type CommandTable = Table (V.Vector RecordType)
type WhereConditionType = WhereCondition ColumnType RecordValueType


toByteStrings :: [String] -> [ByteString]
toByteStrings = map (TE.encodeUtf8 . T.pack)

toStringsV :: V.Vector ByteString -> V.Vector String
toStringsV = V.map (T.unpack . TE.decodeUtf8)

toStrings :: [ByteString] -> [String]
toStrings = map (T.unpack . TE.decodeUtf8)

create :: [ColumnType] -> CommandTable
create colNames = Table $ singleton (Record $ fromList colNames)

insert :: CommandTable -> [ColumnType] -> [Record [RecordValueType]] -> CommandTable
insert (Table recordsV) cols values = let prev@((Record header):records) = (toList recordsV); insertedIn = toList $ V.map (`elem` cols) header in
        Table $ fromList (prev ++ next insertedIn)
    where
        next addVal = map (\(Record vs) -> makeRecord addVal (fromList $ toStrings vs)) values
        makeRecord addVal recordV = Record $ fromList $ toByteStrings $ fillAllZip addVal (toList recordV)

        fillAllZip (f:fZip) l = if f then let (li:lis) = l in (li:fillAllZip fZip lis)
                              else "":fillAllZip fZip l
        fillAllZip [] [] = []
        fillAllZip _ _ = undefined

update :: CommandTable -> [(ColumnType, RecordValueType)] -> WhereConditionType -> CommandTable
update t updates cond = undefined

applyCommand :: CommandTable -> CommandDataType -> CommandTable
applyCommand _ (Create colNames) = create colNames
applyCommand table (Insert colNames rs) = insert table colNames rs
applyCommand table (Update updates condition) = update table updates condition 

emptyTable :: CommandTable
emptyTable = Table empty