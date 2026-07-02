{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE OverloadedStrings#-}

module Commands where

import Data.List (intercalate)
import DataTypes (Record (..), Table (..), WhereCondition (..), CommandData(..), AlterData (..), SetOperation (..))

import Data.Vector (fromList, singleton, empty, toList)
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromJust)
import Data.Csv (header)

type ColumnType = ByteString
type RecordValueType = ByteString
type RecordType = Record (V.Vector RecordValueType)
type Header = RecordType

type CommandDataType = CommandData ColumnType RecordValueType CommandTable
type CommandTable = Table (V.Vector RecordType)
type WhereConditionParsed = WhereCondition ColumnType RecordValueType
type AlterType = AlterData ColumnType

create :: [ColumnType] -> CommandTable
create colNames = Table $ singleton (Record $ fromList colNames)

correspondingIndex :: Header -> ColumnType -> Int -- TODO: what if not present?
correspondingIndex (Record header) columnName = fromJust $ V.findIndex (== columnName) header

whereFunc :: RecordType -> ColumnType -> (RecordValueType -> Bool) -> RecordType -> Bool
whereFunc header col f (Record values) = f (values V.! conditionColumnIndex)
    where
        conditionColumnIndex = correspondingIndex header col

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

divide :: CommandTable -> (Header, V.Vector RecordType)
divide (Table t) = (V.head t, V.tail t)

makeTable :: Header -> V.Vector RecordType -> CommandTable
makeTable header rs = Table $ V.cons header rs

divideWithCond :: CommandTable -> WhereConditionParsed -> (Header, V.Vector RecordType, RecordType -> Bool)
divideWithCond t cond = (header, rs, conditionInterpreted)
    where
        conditionInterpreted = interpretWhereCondition cond header
        (header, rs) = divide t

correspondingIndices :: Header -> V.Vector ColumnType -> V.Vector Int
correspondingIndices (Record header) cols = V.findIndices (`elem` cols) header

colValueToIndexValue :: Header -> V.Vector (ColumnType, RecordValueType) -> V.Vector (Int, RecordValueType)
colValueToIndexValue header mapping = V.zipWith (\i (_, v) -> (i, v)) (correspondingIndices header cols) mapping
    where
        cols = V.map fst mapping

insert :: CommandTable -> [ColumnType] -> [Record [RecordValueType]] -> CommandTable
insert (Table recordsV) cols recs = Table $ recordsV V.++ newValues
    where
        newValues = V.unfoldr (\rs -> if null rs then Nothing else let r:rss = rs in Just (overwriteRecord emptyNewRecordLine (howPutValues r), rss)) recs
        h@(Record header) = V.head recordsV
        emptyNewRecordLine = Record $ V.replicate (V.length header) ""

        howPutValues (Record vals) = V.zip indices $ fromList vals -- TODO: implement checks whether vals has the same length as correspondingIndexes
        indices = correspondingIndices h $ fromList cols

update :: CommandTable -> [(ColumnType, RecordValueType)] -> WhereConditionParsed -> CommandTable
update t updates cond = makeTable header $ V.map updateRecord recs
    where
        updateRecord r = if condition r then overwriteRecord r (colValueToIndexValue header $ fromList updates) else r
        (header, recs, condition) = divideWithCond t cond

delete :: CommandTable -> WhereConditionParsed -> CommandTable
delete table cond = makeTable header $ V.filter (not.condition) recs
    where
        (header, recs, condition) = divideWithCond table cond

select :: CommandTable -> [ColumnType] -> CommandTable
select table cols = makeTable (Record (V.backpermute h indices)) $ V.map (\(Record r) -> Record $ V.backpermute r indices) recs
    where
        (header@(Record h), recs) = divide table
        indices = correspondingIndices header $ fromList cols

alterAdd :: CommandTable -> ColumnType -> CommandTable
alterAdd table columnName = makeTable newHeader newRecords
    where
        newHeader = Record $ h V.++ V.singleton columnName
        newRecords = V.map (\(Record r) -> Record $ V.snoc r "") recs

        (Record h, recs) = divide table

dropAt :: Int -> V.Vector a -> V.Vector a
dropAt index vector = first V.++ V.tail droppedWithSecond
    where
        (first, droppedWithSecond) = V.splitAt index vector

alterDrop :: CommandTable -> ColumnType -> CommandTable
alterDrop table@(Table t) colName = Table $ V.map (\(Record r) -> Record $ dropAt index r) t
    where
        index = correspondingIndex header colName
        (header, _) = divide table

alterRename :: CommandTable -> ColumnType -> ColumnType -> CommandTable
alterRename table current new = makeTable newHeader recs
    where
        newHeader = Record $ V.map (\c -> if c == current then new else c) header
        (Record header, recs) = divide table

applyAlterCommand :: CommandTable -> AlterType -> CommandTable
applyAlterCommand table (Add colName) = alterAdd table colName
applyAlterCommand table (Drop colName) = alterDrop table colName
applyAlterCommand table (Rename current new) = alterRename table current new

applySetOperationCommand :: CommandTable -> CommandTable -> SetOperation -> CommandTable
applySetOperationCommand t1 t2 op = makeTable header $ case op of
        Intersection -> recordsIntersection
        Union -> recordsUnion
        Difference -> recordsDifference
    where
        (header, records1) = divide t1
        (_, records2) = divide t2

        recordsIntersection = takeSatisfying (`elem` records2) records1
        recordsUnion = records1 V.++ takeSatisfying (`notElem` records1) records2
        recordsDifference = takeSatisfying (`notElem` records2) records1

        takeSatisfying f = V.concatMap (\r -> if f r then V.singleton r else V.empty)

applyCommand :: CommandTable -> CommandDataType -> CommandTable
applyCommand _ (Create colNames) = create colNames
applyCommand table (Insert colNames rs) = insert table colNames rs
applyCommand table (Update updates condition) = update table updates condition
applyCommand table (Delete condition) = delete table condition
applyCommand table (Select condition) = select table condition
applyCommand table (Alter subcommand) = applyAlterCommand table subcommand

applyTwoTableCommand :: CommandTable -> CommandTable -> SetOperation -> CommandTable
applyTwoTableCommand = applySetOperationCommand

emptyTable :: CommandTable
emptyTable = Table empty