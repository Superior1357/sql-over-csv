{-# LANGUAGE OverloadedStrings #-}

module LibControl (runCommand, openTable, handleException) where

import LibExceptions
import Parsers (commandParser, ParsedData)
import Commands (applyCommand, CommandTable, CommandDataType, applyTwoTableCommand)
import CommandExceptions (CommandException (..))

import DataTypes

import Text.Megaparsec (runParser, errorBundlePretty)
import Data.Csv (decode, HasHeader (NoHeader), encode)

import qualified Data.Vector as V
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import System.Directory (doesFileExist)
import qualified Data.Bifunctor
import Control.Exception (throw)

toByteString :: String -> ByteString
toByteString = TE.encodeUtf8 . T.pack

toByteStrings :: [String] -> [ByteString]
toByteStrings = map toByteString

castWhereCondition :: WhereCondition String String -> WhereCondition ByteString ByteString
castWhereCondition (Equal a b) = Equal (toByteString a) (toByteString b)
castWhereCondition (Greater a b) = Greater (toByteString a) (toByteString b)
castWhereCondition (GreaterEqual a b) = GreaterEqual (toByteString a) (toByteString b)
castWhereCondition (Less a b) = Less (toByteString a) (toByteString b)
castWhereCondition (LessEqual a b) = LessEqual (toByteString a) (toByteString b)
castWhereCondition (NotEqual a b) = NotEqual (toByteString a) (toByteString b)
castWhereCondition (In a b) = In (toByteString a) (toByteStrings b)
castWhereCondition NoCondition = NoCondition

castAlterData :: AlterData String -> AlterData ByteString
castAlterData (Add column) = Add $ toByteString column
castAlterData (Drop column) = Drop $ toByteString column
castAlterData (Rename old new) = Rename (toByteString old) (toByteString new)

parsedDataToCmdData :: ParsedData -> CommandDataType
parsedDataToCmdData (Create colNames) = Create $ toByteStrings colNames
parsedDataToCmdData (Insert cols recs) = Insert (toByteStrings cols) (map (\(Record vs) -> Record $ toByteStrings vs) recs)
parsedDataToCmdData (Update updates cond) = Update (map (Data.Bifunctor.bimap toByteString toByteString) updates) $ castWhereCondition cond
parsedDataToCmdData (Delete cond) = Delete $ castWhereCondition cond
parsedDataToCmdData (Alter subcommand) = Alter $ castAlterData subcommand
parsedDataToCmdData (Select cols) = Select $ toByteStrings cols

openTable :: FilePath -> IO CommandTable
openTable path = do
    csvText <- readCSVFile
    let strictCsvText = B.fromStrict csvText
    
    case decode NoHeader strictCsvText of
        Right v -> do 
            let table = Table $ V.map Record v
            pure table

        Left err -> throw $ IOTableException err

    where
        readCSVFile = do
            exists <- doesFileExist path
            if exists then do
                B.readFile path
            else
                pure ""

saveTable :: FilePath -> CommandTable -> IO ()
saveTable path (Table t) = do
    let bString = encode $ V.toList stripped
    BL.writeFile path bString

    where
        stripped = V.map (\(Record r) -> r) t

runCommand :: String -> IO ()
runCommand c = do
    case runParser commandParser "" c of
        Right cmd -> go cmd
        Left b -> throw $ ParseException b
    where 
        go (OneTableCmd csvPath parsedData) = do
            resultTable <- getResultTable1 csvPath parsedData
            saveTable csvPath resultTable

        go (TwoTableCmd t1_path t2_path tr_path op) = do
            resultTable <- getResultTable2 t1_path t2_path op
            saveTable tr_path resultTable

        getResultTable1 csvPath parsedData = do
            table <- openTable csvPath
            pure $ applyCommand table $ parsedDataToCmdData parsedData

        getResultTable2 t1_path t2_path op = do
            t1 <- openTable t1_path
            t2 <- openTable t2_path
            pure $ applyTwoTableCommand t1 t2 op

handleException :: ApplicationException -> IO ()
handleException (IOTableException m) = print m
handleException (ParseException b) = putStrLn $ errorBundlePretty b