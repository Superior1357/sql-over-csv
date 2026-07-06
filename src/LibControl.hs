{-# LANGUAGE OverloadedStrings #-}

module LibControl (runCommand, openTable, translateException) where

import LibExceptions
import Parsers (commandParser, ParsedIOCmdData)
import Commands
    ( applyCommand,
      CommandTable,
      applyTwoTableCommand,
      IOCmdData, applyOutputCommand )

import CommandExceptions (CommandException(..))
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
import Control.Exception ( throw, try )

-- | Convert a string to its respective ByteString representation.
toByteString :: String -> ByteString
toByteString = TE.encodeUtf8 . T.pack

-- | Convert a list of Strings to a List of ByteStrings.
toByteStrings :: [String] -> [ByteString]
toByteStrings = map toByteString

-- | Convert a String WHERE condition to a ByteString WHERE condition
castWhereCondition :: WhereCondition String String -> WhereCondition ByteString ByteString
castWhereCondition (Equal a b) = Equal (toByteString a) (toByteString b)
castWhereCondition (Greater a b) = Greater (toByteString a) (toByteString b)
castWhereCondition (GreaterEqual a b) = GreaterEqual (toByteString a) (toByteString b)
castWhereCondition (Less a b) = Less (toByteString a) (toByteString b)
castWhereCondition (LessEqual a b) = LessEqual (toByteString a) (toByteString b)
castWhereCondition (NotEqual a b) = NotEqual (toByteString a) (toByteString b)
castWhereCondition (In a b) = In (toByteString a) (toByteStrings b)
castWhereCondition NoCondition = NoCondition

-- | Convert a String AlterData to a ByteString AlterData.
castAlterData :: AlterData String -> AlterData ByteString
castAlterData (Add column) = Add $ toByteString column
castAlterData (Drop column) = Drop $ toByteString column
castAlterData (Rename old new) = Rename (toByteString old) (toByteString new)

-- | Convert a String based data representation to its respective ByteString representation.
parsedIODataToCmdData :: ParsedIOCmdData -> IOCmdData
parsedIODataToCmdData (Insert cols recs) = Insert (toByteStrings cols) (map (\(Record vs) -> Record $ toByteStrings vs) recs)
parsedIODataToCmdData (Update updates cond) = Update (map (Data.Bifunctor.bimap toByteString toByteString) updates) $ castWhereCondition cond
parsedIODataToCmdData (Delete cond) = Delete $ castWhereCondition cond
parsedIODataToCmdData (Alter subcommand) = Alter $ castAlterData subcommand
parsedIODataToCmdData (Select cols) = Select $ toByteStrings cols

-- | Convert a String based data representation to its respective ByteString representation.
parsedOutputDataToCmdData :: OutputCommandData String -> OutputCommandData ByteString
parsedOutputDataToCmdData (Create colNames) = Create $ toByteStrings colNames

-- | Open a file located on a specified path and try to interpet it 
-- as a CSV file (according to cassava generic representation). 
-- Throws an IOTableException if the file cannot be found or cassava cannot interpret the 
-- file as a CSV file.
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
                throw $ IOTableException path

-- | Save a table to a specified path location.
saveTable :: FilePath -> CommandTable -> IO ()
saveTable path (Table t) = do
    let bString = encode $ V.toList stripped
    BL.writeFile path bString
    where
        stripped = V.map (\(Record r) -> r) t

-- | Interpret a String as an SQL-like command over a CSV file. Then launch it.
-- Throws an ApplicationException if any operation fails.
runCommand :: String -> IO ()
runCommand c = do
    case runParser commandParser "" c of
        Right cmd -> safeGo cmd
        Left b -> throw $ ParseException b
    where
        safeGo cmd = do
            result <- try $ go cmd :: IO (Either CommandException ())
            case result of
                Right r -> pure r
                Left e -> throw $ CmdException e

        go (OneTableCmd csvPath parsedData) = do
            resultTable <- getResultTable1 csvPath parsedData
            saveTable csvPath resultTable

        go (TwoTableCmd t1_path t2_path tr_path op) = do
            resultTable <- getResultTable2 t1_path t2_path op
            saveTable tr_path resultTable

        getResultTable1 csvPath parsedData = do
            case parsedData of
                (IOCmd cmd) -> do
                    table <- openTable csvPath
                    pure $ applyCommand table $ parsedIODataToCmdData cmd

                (OutputCmd cmd) -> pure $ applyOutputCommand $ parsedOutputDataToCmdData cmd


        getResultTable2 t1_path t2_path op = do
            t1 <- openTable t1_path
            t2 <- openTable t2_path
            pure $ applyTwoTableCommand t1 t2 op

-- | Convert an ApplicationException to its respective String representation
translateException :: ApplicationException -> String
translateException (IOTableException m) = m
translateException (ParseException b) = errorBundlePretty b
translateException (CmdException e) = translateCmdException e
    where
        translateCmdException = show