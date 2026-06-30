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
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromJust)

type ColumnType = ByteString
type RecordValueType = ByteString
type RecordType = Record (V.Vector RecordValueType)
type Header = RecordType

type CommandDataType = CommandData ColumnType RecordValueType
type CommandTable = Table (V.Vector RecordType)
type WhereConditionParsed = WhereCondition ColumnType RecordValueType

toByteStrings :: [String] -> [ByteString]
toByteStrings = map (TE.encodeUtf8 . T.pack)

toStringsV :: V.Vector ByteString -> V.Vector String
toStringsV = V.map (T.unpack . TE.decodeUtf8)

toStrings :: [ByteString] -> [String]
toStrings = map (T.unpack . TE.decodeUtf8)

create :: [ColumnType] -> CommandTable
create colNames = Table $ singleton (Record $ fromList colNames)

whereFunc :: RecordType -> ColumnType -> (RecordValueType -> Bool) -> RecordType -> Bool
whereFunc (Record header) col f (Record values) = f (values V.! conditionColumnIndex)
    where
        conditionColumnIndex = fromJust $ V.findIndex (== col) header -- TODO: what if not present?

fIntInterpreted :: RecordValueType -> (Int -> Int -> Bool) -> RecordValueType -> Bool
fIntInterpreted a f b = f (interpretInt a) (interpretInt b)
    where
        interpretInt i = fst $ fromJust $ B8.readInt i

interpretWhereCondition :: WhereConditionParsed -> Header -> (RecordType -> Bool)
interpretWhereCondition NoCondition _ = const True
interpretWhereCondition (Equal c v) h = whereFunc h c (== v)
interpretWhereCondition (NotEqual c v) h = whereFunc h c (/= v)
interpretWhereCondition (Greater c v) h = whereFunc h c $ fIntInterpreted v (<)
interpretWhereCondition (GreaterEqual c v) h = whereFunc h c $ fIntInterpreted v (<=)
interpretWhereCondition (Less c v) h = whereFunc h c $ fIntInterpreted v (>)
interpretWhereCondition (LessEqual c v) h = whereFunc h c $ fIntInterpreted v (>=)
interpretWhereCondition (In c vals) h = whereFunc h c (`elem` vals)

overwriteRecord :: RecordType -> V.Vector (Int, RecordValueType) -> RecordType
overwriteRecord (Record vs) mapping = Record $ V.update vs mapping

colValueToIndexValue :: Header -> V.Vector (ColumnType, RecordValueType) -> V.Vector (Int, RecordValueType)
colValueToIndexValue (Record header) mapping = V.zipWith (\i (_, v) -> (i, v)) correspondingIndices mapping
    where
        correspondingIndices = V.findIndices (`elem` cols) header
        cols = V.map fst mapping

insert :: CommandTable -> [ColumnType] -> [Record [RecordValueType]] -> CommandTable
insert (Table recordsV) cols recs = Table $ recordsV V.++ newValues
    where
        newValues = V.unfoldr (\rs -> if null rs then Nothing else let r:rss = rs in Just (overwriteRecord emptyNewRecordLine (howPutValues r), rss)) recs
        (Record header) = V.head recordsV
        emptyNewRecordLine = Record $ V.replicate (V.length header) ""

        howPutValues (Record vals) = V.zip correspondingIndices $ fromList vals -- TODO: implement checks whether vals has the same length as correspondingIndexes
        correspondingIndices = V.findIndices (`elem` cols) header

update :: CommandTable -> [(ColumnType, RecordValueType)] -> WhereConditionParsed -> CommandTable
update (Table t) updates cond = Table $ V.singleton header V.++ V.map updateRecord (V.tail t)
    where
        updateRecord r = if conditionInterpreted r then overwriteRecord r (colValueToIndexValue header $ fromList updates) else r
        conditionInterpreted = interpretWhereCondition cond header
        header = V.head t

applyCommand :: CommandTable -> CommandDataType -> CommandTable
applyCommand _ (Create colNames) = create colNames
applyCommand table (Insert colNames rs) = insert table colNames rs
applyCommand table (Update updates condition) = update table updates condition

emptyTable :: CommandTable
emptyTable = Table empty