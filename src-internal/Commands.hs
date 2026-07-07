{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE OverloadedStrings#-}

module Commands where

import DataTypes
import Data.Vector (fromList, singleton)
import qualified Data.Vector as V
import Data.ByteString (ByteString, toStrict)
import qualified Data.ByteString.Char8 as B8
import CommandExceptions (CommandException(..))
import Control.Exception (throw)
import Data.ByteString.Builder (intDec, toLazyByteString)

import Data.List (group, sort)

type ColumnType = ByteString
type RecordValueType = ByteString
type RecordType = Record (V.Vector RecordValueType)
type Header = RecordType

type CommandDataType = CommandData ColumnType RecordValueType CommandTable
type CommandTable = Table (V.Vector RecordType)

type OutputCmdData = OutputCommandData ColumnType
type IOCmdData = IOCommandData ColumnType RecordValueType CommandTable
type WhereConditionParsed = WhereCondition ColumnType RecordValueType
type AlterType = AlterData ColumnType

-- | Convert an Int to its ByteString representation.
intToByteString :: Int -> ByteString
intToByteString = toStrict.toLazyByteString.intDec

-- | Create a new CSV table from a given column names list.
-- Throws an exception if there are duplicit column names.
create :: [ColumnType] -> CommandTable
create [] = throw $ InvalidArgCountException "0"
create colNames = case checkColumnsForDuplicate colNames of
                    Nothing -> Table $ singleton (Record $ fromList colNames)
                    Just err -> throw err

-- | Find a corresponding index for a column name in a header.
correspondingIndex :: Header -> ColumnType -> Int
correspondingIndex (Record header) colName = case V.findIndex (== colName) header of
    Just i -> i
    Nothing -> throw $ ColumnNotFoundException colName

-- | A helper function for interpretWhereCondition.
-- Find a column specified in the condition. For every a record evaluate, whether the condition holds at the corresponding index.
whereFunc :: Header -> ColumnType -> (RecordValueType -> Bool) -> RecordType -> Bool
whereFunc header col f (Record values) = f (values V.! conditionColumnIndex)
    where
        conditionColumnIndex = correspondingIndex header col

-- | Interpret two record values as ints and apply a given function on them.
-- Throws an UnableToInterpretException if a values cannot be interpreted as an int.
fIntInterpreted :: RecordValueType -> (Int -> Int -> Bool) -> RecordValueType -> Bool
fIntInterpreted a f b = f (interpretInt a) (interpretInt b)
    where
        interpretInt i = case B8.readInt i of
            Just (v, "") -> v
            _ -> throw $ UnableToInterpretException i

-- | Translate a parsed where condition to an actual function that checks whether the condition is satisfied for a given record.
interpretWhereCondition :: WhereConditionParsed -> Header -> (RecordType -> Bool)
interpretWhereCondition NoCondition _ = const True
interpretWhereCondition (Equal c v) h = whereFunc h c (== v)
interpretWhereCondition (NotEqual c v) h = whereFunc h c (/= v)
interpretWhereCondition (Greater c v) h = whereFunc h c $ fIntInterpreted v (<)
interpretWhereCondition (GreaterEqual c v) h = whereFunc h c $ fIntInterpreted v (<=)
interpretWhereCondition (Less c v) h = whereFunc h c $ fIntInterpreted v (>)
interpretWhereCondition (LessEqual c v) h = whereFunc h c $ fIntInterpreted v (>=)
interpretWhereCondition (In c vals) h = whereFunc h c (`elem` vals)

-- | Change all indices in a vector as specified in mapping.
overwriteRecord :: RecordType -> V.Vector (Int, RecordValueType) -> RecordType
overwriteRecord (Record vs) mapping = Record $ V.update vs mapping

-- | Split a table into a tuple of header and records
divide :: CommandTable -> (Header, V.Vector RecordType)
divide (Table t) = (V.head t, V.tail t)

-- Create a table from header and vector of records
makeTable :: Header -> V.Vector RecordType -> CommandTable
makeTable header rs = Table $ V.cons header rs

-- | Split a table into a tuple of header, records and where condition
divideWithCond :: CommandTable -> WhereConditionParsed -> (Header, V.Vector RecordType, RecordType -> Bool)
divideWithCond t cond = (header, rs, conditionInterpreted)
    where
        conditionInterpreted = interpretWhereCondition cond header
        (header, rs) = divide t

-- | Find corresponding indices for specified column names.
correspondingIndices :: Header -> V.Vector ColumnType -> V.Vector Int
correspondingIndices header = V.map (correspondingIndex header)

-- | Convert a mapping from column names to record values to a mapping from corresponding column indices to record values.
colValueToIndexValue :: Header -> V.Vector (ColumnType, RecordValueType) -> V.Vector (Int, RecordValueType)
colValueToIndexValue header mapping = V.zipWith (\i (_, v) -> (i, v)) (correspondingIndices header cols) mapping
    where
        cols = V.map fst mapping

-- | Return a ColumnNameDuplicatedException iff there are duplicit column names.
checkColumnsForDuplicate :: [ColumnType] -> Maybe CommandException
checkColumnsForDuplicate cols = case duplicated of
        [] -> Nothing
        (e:_) -> Just $ ColumnNameDuplicatedException e
    where
        duplicated = map snd $ dropWhile fst $ map (\g -> (null (tail g), head g)) (group $ sort cols)

-- | Insert new values to specified columns. Insert null values to other columns.
-- Throws a ColumnNotFoundException if a specified column name cannot be found.
-- Throws a ColumnNameDuplicatedException if trying to insert two values to a column.
-- Throws an InvalidArgCount exception if trying to insert a different number of values than the columns specified.
insert :: CommandTable -> [ColumnType] -> [Record [RecordValueType]] -> CommandTable
insert (Table recordsV) cols recs = case checkColumnsForDuplicate cols of
                                        Nothing -> Table $ recordsV V.++ newValues
                                        Just exc -> throw exc
    where
        newValues = V.unfoldr (\rs -> if null rs then Nothing else Just (overwriteRecord emptyNewRecordLine (howPutValues $ head rs), tail rs)) recs
        h@(Record header) = V.head recordsV
        emptyNewRecordLine = Record $ V.replicate (V.length header) ""

        howPutValues (Record vals) = let valsV = fromList vals; lengthVals = V.length valsV in if V.length indices == lengthVals then V.zip indices valsV else throw $ InvalidArgCountException $ intToByteString lengthVals
        indices = correspondingIndices h $ fromList cols

-- | Change a column value for all records where a specified condition holds.
-- Throws a ColumnNotFoundException if the specified column name cannot be found.
-- Throws an UnableToInterpretException if a value cannot be applied to the specified condition.
update :: CommandTable -> [(ColumnType, RecordValueType)] -> WhereConditionParsed -> CommandTable
update t updates parsedCond = case checkColumnsForDuplicate $ map fst updates of
                                Nothing -> makeTable header $ V.map updateRecord recs
                                Just exc -> throw exc
    where
        updateRecord r = if cond r then overwriteRecord r (colValueToIndexValue header $ fromList updates) else r
        (header, recs, cond) = divideWithCond t parsedCond

-- | Delete all records where a specified condition holds.
-- Throws a ColumnNotFoundException if the specified column name cannot be found.
-- Throws an UnableToInterpretException if a value cannot be applied to the specified condition. 
delete :: CommandTable -> WhereConditionParsed -> CommandTable
delete table parsedCond = makeTable header $ V.filter (not.cond) recs
    where
        (header, recs, cond) = divideWithCond table parsedCond

-- | Select specified columns from the table (in the command ordering).
-- Throws a ColumnNotFoundException if the column name doesn't exist.
select :: CommandTable -> [ColumnType] -> CommandTable
select table cols = case checkColumnsForDuplicate cols of
    Nothing -> makeTable (Record (V.backpermute h indices)) $ V.map (\(Record r) -> Record $ V.backpermute r indices) recs
    Just exc -> throw exc
    where
        (header@(Record h), recs) = divide table
        indices = correspondingIndices header $ fromList cols

-- | Add a new column with the given name to the table. Add all values as null.
-- Throws a ColumnNameDuplicatedException if the column already exists.
alterAdd :: CommandTable -> ColumnType -> CommandTable
alterAdd table colName = if colName `V.notElem` h then makeTable newHeader newRecords else throw $ ColumnNameDuplicatedException colName
    where
        newHeader = Record $ h V.++ V.singleton colName
        newRecords = V.map (\(Record r) -> Record $ V.snoc r "") recs

        (Record h, recs) = divide table

-- | Remove an item on the specified index from the vector.
dropAt :: Int -> V.Vector a -> V.Vector a
dropAt index vector = first V.++ V.tail droppedWithSecond
    where
        (first, droppedWithSecond) = V.splitAt index vector

-- | Remove a specified column from the table.
-- Throws an ColumnNotFoundException if the column cannot be found.
alterDrop :: CommandTable -> ColumnType -> CommandTable
alterDrop table@(Table t) colName = Table $ V.map (\(Record r) -> Record $ dropAt index r) t
    where
        index = correspondingIndex header colName
        (header, _) = divide table

-- | Rename a table's column.
-- Throws a ColumnNotFoundException if the column cannot be found.
-- Throws a ColumnNameDuplicatedException if the rename creates a duplicate column name.
alterRename :: CommandTable -> ColumnType -> ColumnType -> CommandTable
alterRename table current new = case makeChecks of
    Nothing -> makeTable newHeader recs
    Just exc -> throw exc
    where
        newHeader = Record $ V.map (\c -> if c == current then new else c) header
        (Record header, recs) = divide table
        
        makeChecks
            | current `notElem` header = Just $ ColumnNotFoundException current
            | new `elem` header = Just $ ColumnNameDuplicatedException new
            | otherwise = Nothing

-- | Call a correct alter command function.
applyAlterCommand :: CommandTable -> AlterType -> CommandTable
applyAlterCommand table (Add colName) = alterAdd table colName
applyAlterCommand table (Drop colName) = alterDrop table colName
applyAlterCommand table (Rename current new) = alterRename table current new

-- | Apply a set operation command on the specified table of the same type.
-- The supported commands are union, intersection and difference.
-- Throws an exception if two input tables have different format (different column names).
applySetOperationCommand :: CommandTable -> CommandTable -> SetOperation -> CommandTable
applySetOperationCommand t1 t2 op = if header1 == header2 then go else findFirstNonMatching
    where
        findFirstNonMatching = throw $ HeadersDifferException $ fst $ head $ dropWhile (uncurry (==)) $ zip (V.toList header1 ++ repeat "") (V.toList header2 ++ repeat "")

        go = makeTable h $ case op of
            Intersection -> recordsIntersection
            Union -> recordsUnion
            Difference -> recordsDifference

        (h@(Record header1), records1) = divide t1
        (Record header2, records2) = divide t2

        recordsIntersection = takeSatisfying (`elem` records2) records1
        recordsUnion = records1 V.++ takeSatisfying (`notElem` records1) records2
        recordsDifference = takeSatisfying (`notElem` records2) records1

        takeSatisfying f = V.concatMap (\r -> if f r then V.singleton r else V.empty)

-- | Check whether the input table's format is supported.
-- Returns an InvalidTableFormatException if:
--      - the table is empty
--      - the header is empty
--      - a record's length doesn't match the the header's length
--      - there are duplicate column names in the header
checkTable :: CommandTable -> Maybe CommandException
checkTable tab@(Table t) = if tableEmpty then Just $ InvalidTableFormatException "0" else
                       let (Record header, recs) = divide tab; colCount = V.length header in
                        if colCount == 0 then Just $ InvalidTableFormatException "0" else
                        case checkColumnsForDuplicate $ V.toList header of
                            Nothing -> tryGetFirstRecordOfUnMatchingSize colCount recs
                            Just (ColumnNameDuplicatedException _) -> Just $ InvalidTableFormatException "0"
                            Just err -> throw err

    where
        tableEmpty = V.length t == 0
        tryGetFirstRecordOfUnMatchingSize size recs = case dropWhile (\(_, Record r) -> V.length r == size) $ zip [1..] (V.toList recs) of
            [] -> Nothing
            ((i, _):_) -> Just $ InvalidTableFormatException $ intToByteString i

-- | Apply a specified command on a given CommandTable. 
-- Throws an InvalidTableFormatException if the CommandTable's format is not supported.
-- Throws a CommandException if specified command data is invalid.
applyCommand :: CommandTable -> IOCmdData -> CommandTable
applyCommand tableUnsafe = case checkTable tableUnsafe of
    Nothing -> go tableUnsafe
    Just err -> throw err
    where
        go table (Insert colNames rs) = insert table colNames rs
        go table (Update updates cond) = update table updates cond
        go table (Delete cond) = delete table cond
        go table (Select cond) = select table cond
        go table (Alter subcommand) = applyAlterCommand table subcommand

-- | Apply a command that doesn't need a table as input.
-- Throws a CommandException if the command is invalid.
applyOutputCommand :: OutputCmdData -> CommandTable
applyOutputCommand (Create colNames) = if null colNames then throw $ InvalidArgCountException "0" else create colNames

applyTwoTableCommand :: CommandTable -> CommandTable -> SetOperation -> CommandTable
applyTwoTableCommand t1 t2 = case checkTable t1 of
    Nothing -> case checkTable t2 of
        Nothing -> applySetOperationCommand t1 t2
        Just exc -> throw exc
    Just exc -> throw exc
    
    