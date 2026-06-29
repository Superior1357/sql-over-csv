{-# LANGUAGE OverloadedStrings #-}

module LibControl (runCommand, openTable) where
import Parsers (commandParser, ParsedData, ParsedCommand)
import Commands (applyCommand, CommandTable, CommandDataType)
import DataTypes   

import Text.Megaparsec (runParser)
import Data.Csv (decode, HasHeader (NoHeader), encode)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL

import System.Directory (doesFileExist)

parsedDataToCmdData :: ParsedData -> CommandDataType
parsedDataToCmdData  = undefined

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
    let (Cmd csvPath parsedData) = parseCommand c
    table <- openTable csvPath
    let newTable = applyCommand table $ parsedDataToCmdData parsedData
    saveTable csvPath newTable