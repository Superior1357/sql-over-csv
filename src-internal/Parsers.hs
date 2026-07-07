module Parsers (Command, commandParser, whereParser, parseWord, ParsedData, ParsedIOCmdData, ParsedCommand, parseList, parseDictionary) where
import Data.Void (Void)
import Text.Megaparsec (Parsec, some, sepBy, sepBy1, choice, satisfy)
import Text.Megaparsec.Char (string, space1, char, space)
import Control.Applicative (many, (<|>))
import Data.Char (isSpace)
import DataTypes

import Data.Functor (($>))

type Parser = Parsec Void String

type Column = String
type RecordValue = String
type ParseTable = FilePath

type ParsedOutputCmdData = OutputCommandData Column
type ParsedIOCmdData = IOCommandData Column RecordValue ParseTable

type ParsedData = CommandData Column RecordValue ParseTable
type ParsedCommand = Command ParsedData SetOperation

-- | Check whether a character is a special symbol.
specialSymbol :: Char -> Bool
specialSymbol s = isSpace s || s `elem` symbols
    where
        symbols = ['"', ',', '(', ')', ';', '=']

-- | The supported file path is the same as any word format.
supportedFilePath :: Parser FilePath
supportedFilePath = parseWord

-- | Parse data in the following format: (V1, V2, ...)
parseList :: Parser [String]
parseList = char '(' *> space *> parseWord `sepBy` (space *> char ',' <* space) <* space <* char ')'

-- | Parse any string of characters if enclosed in "" (the quote itself has to be written twice).
-- Parse a string of nonspecial characters.
parseWord :: Parser String
parseWord = (char '"' *> enclosed <* char '"') <|> open
    where
        enclosed = many (satisfy (/= '"') <|> concatQuotes)
        concatQuotes = string "\"\"" $> '"'
        open = some $ satisfy $ not.specialSymbol

-- | Parse a WHERE condition.
-- comparison condition format (* `elem` [=, >, <, >=, <=, <>]): columnName * value
-- belong to a set of values format: columnName IN (V1, V2, V3)
-- no condition format - empty
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

-- | parse data in the following format: (column1 = value1, column2 = value2, ...)
parseDictionary :: Parser [(Column, RecordValue)]
parseDictionary = char '(' *> space *> (parsePair `sepBy` char ',') <* space <* char ')'
    where
        parsePair = space *> ((,) <$> (parseWord <* space <* char '=' <* space) <*> parseWord) <* space

-- | Parsed for the CREATE command data
-- parsed command data format: (column1_name, column2_name,...,columnN_name);
createParser :: Parser ParsedOutputCmdData
createParser = space1 *> (Create <$> parseList)

-- | Parser for the ALTER command data (ALTER ADD, ALTER DROP, ALTER RENAME).
-- parsed command data format
--      ADD: ADD column_name;
--      DROP: DROP COLUMN column_name;
--      RENAME: RENAME COLUMN old_column_name TO new_column_name;
alterParser :: Parser ParsedIOCmdData
alterParser = space1 *> (Alter <$> choice [
        string "ADD" >> space1 >> addParser,
        string "DROP" >> space1 >> string "COLUMN" >> space1 >> dropParser,
        string "RENAME" >> space1 >> string "COLUMN" >> space1 >> renameParser
    ])
    where
        addParser = Add <$> parseWord
        dropParser = Drop <$> parseWord
        renameParser = Rename <$> parseWord <* space1 <* string "TO" <* space1 <*> parseWord

-- | Parser for the INSERT command data
-- parsed command data format: (column1 column2, column3, ...) VALUES (value11, value12, value13, ...), (value21, value22, value23, ...);
insertParser :: Parser ParsedIOCmdData
insertParser = space1 *> (Insert <$> parseList <* space1 <* string "VALUES" <* space1 <*> recordList)
    where
        recordList = map Record <$> (parseList `sepBy1` (space *> char ',' <* space))

-- | Parser for the UPDATE command data
-- parsed command data format: SET (column1 = value1, column2 = value2, ...) WHERE condition;
updateParser :: Parser ParsedIOCmdData
updateParser = space1 *> string "SET" *> space1 *> (Update <$> parseDictionary <* space <*> whereParser)

-- | Parser for the DELETE command data
-- parsed command data format: table_name WHERE condition;
deleteParser :: Parser ParsedIOCmdData
deleteParser = space1 *> (Delete <$> whereParser)

-- | Parser for the SELECT command
-- parsed command data: (column1, column2, ...) FROM table_name;
selectParser :: Parser ParsedCommand
selectParser = do
    colNames <- parseList
    space1
    _ <- string "FROM"
    space1
    tableName <- supportedFilePath
    pure $ OneTableCmd tableName $ IOCmd $ Select colNames

-- | Parser for all supported commands as given in the specification.
-- Parse the command beginning first sometimes the following space, then parse the command's data.
commandParser :: Parser ParsedCommand
commandParser = space *> choice [
                    string "CREATE" *> assembleOneTableCmd (OutputCmd <$> createParser),
                    string "INSERT" *> space1 *> string "INTO" *> assembleOneTableCmd (IOCmd <$> insertParser),
                    string "UPDATE" *> assembleOneTableCmd (IOCmd <$> updateParser),
                    string "DELETE" *> space1 *> string "FROM" *> assembleOneTableCmd (IOCmd <$> deleteParser),
                    string "ALTER" *> assembleOneTableCmd (IOCmd <$> alterParser),
                    string "SELECT" *> space1 *> selectParser,
                    string "UNION" *> assembleTwoTableCmd Union,
                    string "INTERSECTION" *> assembleTwoTableCmd Intersection,
                    string "DIFFERENCE" *> assembleTwoTableCmd Difference
                ] <* space <* char ';'
    where
        assembleOneTableCmd commandDataParser = space1 *> (OneTableCmd <$> supportedFilePath <*> commandDataParser)
        assembleTwoTableCmd ctor = space1 *> (TwoTableCmd <$> supportedFilePath <*> (char ',' *> space *> parseWord <* space1 <* string "INTO" <* space1) <*> parseWord <*> pure ctor)