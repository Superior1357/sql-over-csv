{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE OverloadedStrings#-}

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
insert (Table recordsV) cols recs = Table $ recordsV V.++ newValues
    where
        newValues = V.unfoldr (\rs -> if null rs then Nothing else let r:rss = rs in Just (filledLine r, rss)) recs
        (Record header) = V.head recordsV
        filledLine vals = Record $ emptyNewRecordLine V.// howPutValues vals
        emptyNewRecordLine = V.replicate (V.length header) ""

        howPutValues (Record vals) = zip correspondingIndexes vals -- TODO: implement checks whether vals has the same length as correspondingIndexes
        correspondingIndexes = map fst $ filter (\(_, name) -> name `elem` cols) $ zip [0..] (toList header)

-- TODO: 
update :: CommandTable -> [(ColumnType, RecordValueType)] -> WhereConditionType -> CommandTable
update (Table t) updates cond = undefined

applyCommand :: CommandTable -> CommandDataType -> CommandTable
applyCommand _ (Create colNames) = create colNames
applyCommand table (Insert colNames rs) = insert table colNames rs
applyCommand table (Update updates condition) = update table updates condition

emptyTable :: CommandTable
emptyTable = Table empty