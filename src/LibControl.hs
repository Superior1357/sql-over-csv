{-# LANGUAGE OverloadedStrings #-}

module LibControl (runCommand, openTable) where
import Parsers (commandParser, ParsedData, ParsedCommand)
import Commands (applyCommand, CommandTable, CommandDataType, applyTwoTableCommand)
import DataTypes

import Text.Megaparsec (runParser)
import Data.Csv (decode, HasHeader (NoHeader), encode)

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import System.Directory (doesFileExist)
import qualified Data.Bifunctor

toByteString :: String -> ByteString
toByteString = TE.encodeUtf8 . T.pack

toString :: ByteString -> String
toString = T.unpack . TE.decodeUtf8

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
parsedDataToCmdData (Insert columns records) = Insert (toByteStrings columns) (map (\(Record vs) -> Record $ toByteStrings vs) records)
parsedDataToCmdData (Update updates condition) = Update (map (Data.Bifunctor.bimap toByteString toByteString) updates) $ castWhereCondition condition
parsedDataToCmdData (Delete condition) = Delete $ castWhereCondition condition
parsedDataToCmdData (Alter subcommand) = Alter $ castAlterData subcommand
parsedDataToCmdData (Select columns) = Select $ toByteStrings columns

parseCommand :: String -> ParsedCommand
parseCommand cmd = case runParser commandParser "" cmd of
    Right c -> c

openTable :: FilePath -> IO CommandTable
openTable path = do
    csvText <- readCSVFile
    let Right v = decode NoHeader csvText

    let table = Table $ V.map Record v
    pure table
    where
        readCSVFile = do
            exists <- doesFileExist path
            if exists then do
                BL.readFile path
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
    case parseCommand c of
        (OneTableCmd csvPath parsedData) -> do 
            table <- openTable csvPath
            let newTable = applyCommand table $ parsedDataToCmdData parsedData
            saveTable csvPath newTable
        (TwoTableCmd t1_path t2_path tr_path op) -> do
            t1 <- openTable t1_path
            t2 <- openTable t2_path
            let newTable = applyTwoTableCommand t1 t2 op
            saveTable tr_path newTable