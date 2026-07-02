module Parsers (Command, commandParser, whereParser, parseWord, ParsedData, ParsedCommand) where
import Data.Void (Void)
import Text.Megaparsec (Parsec, some, sepBy1, choice, satisfy)
import Text.Megaparsec.Char (string, space1, char, space)
import Control.Applicative (many, (<|>))
import Data.Char (isSpace)
import DataTypes

import Data.Functor (($>))

type Parser = Parsec Void String

type Column = String
type RecordValue = String

type ParsedData = CommandData Column RecordValue FilePath
type ParsedCommand = Command ParsedData SetOperation

specialSymbol :: Char -> Bool
specialSymbol s = isSpace s || s `elem` symbols
    where
        symbols = ['"', ',', '(', ')', ';']

someExcept :: [Char] -> Parser String
someExcept cs = some $ satisfy (`notElem` cs)

supportedFilePath :: Parser FilePath
supportedFilePath = parseWord

parseList :: Parser [String]
parseList = char '(' *> parseWord `sepBy1` char ',' <* char ')'

parseWord :: Parser String
parseWord = (char '"' *> enclosed <* char '"') <|> open
    where
        enclosed = many (satisfy (/= '"') <|> concatQuotes)
        concatQuotes = string "\"\"" $> '"'
        open = many $ satisfy $ not.specialSymbol

whereParser :: Parser (WhereCondition Column RecordValue)
whereParser = (string "WHERE" *> space1 *> whereCondition) <|>
              pure NoCondition
    where
        whereCondition = do
            colName <- parseWord
            space1
            operator colName

        operator colName = choice [ -- adhere the order of options here
                string ">=" *> buildWithSecondArg GreaterEqual colName,
                string "<=" *> buildWithSecondArg LessEqual colName,
                string "<>" *> buildWithSecondArg NotEqual colName,
                string "IN" *> space1 *> (In colName <$> parseList),
                char '=' *> buildWithSecondArg Equal colName,
                char '>' *> buildWithSecondArg Greater colName,
                char '<' *> buildWithSecondArg Less colName
            ]

        buildWithSecondArg constructor colName = constructor colName <$> (space1 *> parseWord)

parseDictionary :: Parser [(Column, RecordValue)]
parseDictionary = char '(' *> (parsePair `sepBy1` char ',') <* char ')'
    where
        parsePair = (,) <$> (parseWord <* space1 <* char '=' <* space1) <*> parseWord

createParser :: Parser ParsedData
createParser = space1 *> (Create <$> parseList)

alterParser :: Parser ParsedData
alterParser = space1 *> (Alter <$> choice [
        string "ADD" >> space1 >> addParser,
        string "DROP" >> space1 >> string "COLUMN" >> space1 >> dropParser,
        string "RENAME" >> space1 >> string "COLUMN" >> space1 >> renameParser
    ])
    where
        addParser = Add <$> parseWord
        dropParser = Drop <$> parseWord
        renameParser = Rename <$> parseWord <* space1 <* string "TO" <* space1 <*> parseWord

insertParser :: Parser ParsedData
insertParser = space1 *> (Insert <$> parseList <* space1 <* string "VALUES" <* space1 <*> recordList)
    where
        recordList = map Record <$> (parseList `sepBy1` char ',')

updateParser :: Parser ParsedData
updateParser = space1 *> string "SET" *> space1 *> (Update <$> parseDictionary <* space <*> whereParser)

deleteParser :: Parser ParsedData
deleteParser = space1 *> (Delete <$> whereParser)

selectParser :: Parser ParsedCommand
selectParser = do
    colNames <- parseList
    space1
    _ <- string "FROM"
    space1
    tableName <- supportedFilePath
    pure $ OneTableCmd tableName $ Select colNames

commandParser :: Parser ParsedCommand
commandParser = choice [
                    string "CREATE" *> assembleOneTableCmd createParser,
                    string "INSERT" *> space1 *> string "INTO" *> assembleOneTableCmd insertParser,
                    string "UPDATE" *> assembleOneTableCmd updateParser,
                    string "DELETE" *> space1 *> string "FROM" *> assembleOneTableCmd deleteParser,
                    string "ALTER" *> assembleOneTableCmd alterParser,
                    string "SELECT" *> space1 *> selectParser,
                    string "UNION" *> assembleTwoTableCmd Union,
                    string "INTERSECTION" *> assembleTwoTableCmd Intersection,
                    string "DIFFERENCE" *> assembleTwoTableCmd Difference
                ] <* space <* char ';'
    where
        assembleOneTableCmd commandDataParser = space1 *> (OneTableCmd <$> supportedFilePath <*> commandDataParser)
        assembleTwoTableCmd ctor = space1 *> (TwoTableCmd <$> supportedFilePath <*> (char ',' *> space *> parseWord <* space1 <* string "INTO" <* space1) <*> parseWord <*> pure ctor)