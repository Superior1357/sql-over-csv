module Parsers (Command, commandParser, whereParser, ParsedData, ParsedCommand) where
import Data.Void (Void)
import Text.Megaparsec (Parsec, some, sepBy1, choice, satisfy)
import Text.Megaparsec.Char (string, space1, char, space)
import Control.Applicative (many, (<|>))
import Data.Char (isSpace)
import DataTypes

type Parser = Parsec Void String

type Column = String
type RecordValue = String

type ParsedData = CommandData Column RecordValue
type ParsedCommand = Command ParsedData

someExcept :: [Char] -> Parser String
someExcept cs = some $ satisfy (`notElem` cs)

manyExcept :: [Char] -> Parser String
manyExcept cs = many $ satisfy (`notElem` cs)

supportedFilePath :: Parser FilePath
supportedFilePath = someExcept [' ', ';']

parseList :: Parser [String]
parseList = char '(' *> manyExcept [',', '(', ')'] `sepBy1` char ',' <* char ')'

parseWord :: Parser String
parseWord = many $ satisfy (\c -> (not.isSpace) c && (c /= ';'))

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
parseDictionary = undefined

alterDataParser :: Parser (AlterData Column)
alterDataParser = undefined

createParser :: Parser ParsedData
createParser = Create <$> parseList

alterParser :: Parser ParsedData
alterParser = Alter <$> choice [
        string "ADD" >> space1 >> addParser,
        string "DROP" >> space1 >> string "COLUMN" >> space1 >> dropParser,
        string "RENAME" >> space1 >> string "COLUMN" >> space1 >> renameParser
    ]
    where
        addParser = Add <$> parseWord
        dropParser = Drop <$> parseWord
        renameParser = Rename <$> parseWord <* space1 <* string "TO" <* space1 <*> parseWord

insertParser :: Parser ParsedData
insertParser = Insert <$> parseList <* space1 <* string "VALUES" <* space1 <*> recordList
    where
        recordList = map Record <$> (parseList `sepBy1` char ',')

updateParser :: Parser ParsedData
updateParser = string "SET" *> space1 *> (Update <$> parseDictionary <* space1 <*> whereParser)

deleteParser :: Parser ParsedData
deleteParser = Delete <$> whereParser

selectParser :: Parser ParsedCommand
selectParser = do
    colNames <- parseList
    space1
    _ <- string "FROM"
    space1
    tableName <- supportedFilePath
    pure $ Cmd tableName $ Select colNames

unionParser :: Parser ParsedData
unionParser = undefined

intersectionParser :: Parser ParsedData
intersectionParser = undefined

differenceParser :: Parser ParsedData
differenceParser = undefined

commandParser :: Parser ParsedCommand
commandParser = choice [
                    string "CREATE" *> assembleWith createParser,
                    string "INSERT" *> space1 *> string "INTO" *> assembleWith insertParser,
                    string "UPDATE" *> assembleWith updateParser,
                    string "DELETE" *> assembleWith deleteParser,
                    string "ALTER" *> assembleWith alterParser,
                    string "UNION" *> assembleWith unionParser,
                    string "INTERSECTION" *> assembleWith intersectionParser,
                    string "DIFFERENCE" *> assembleWith differenceParser,
                    string "SELECT" *> space1 *> selectParser
                ] <* space <* char ';'
    where
        assembleWith commandDataParser = space1 *> (Cmd <$> supportedFilePath <* space1 <*> commandDataParser)