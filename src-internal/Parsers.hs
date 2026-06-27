module Parsers (Command, commandParser) where
import Data.Void (Void)
import Text.Megaparsec (Parsec, some, sepBy1, choice, satisfy)
import Text.Megaparsec.Char (string, space1, char, space)
import ParsingTypes
import Control.Applicative (many)
import Data.Char (isSpace)

type Parser = Parsec Void String

someExcept :: [Char] -> Parser String
someExcept cs = some $ satisfy (`notElem` cs)

manyExcept :: [Char] -> Parser String
manyExcept cs = many $ satisfy (`notElem` cs)

supportedFilePath :: Parser FilePath
supportedFilePath = someExcept [' ']

parseList :: Parser [String]
parseList = char '(' *> manyExcept [',', '(', ')'] `sepBy1` char ',' <* char ')'

parseWord :: Parser String
parseWord = many $ satisfy (\c -> (not.isSpace) c && (c /= ';'))

whereParser :: Parser WhereCondition
whereParser = undefined

parseDictionary :: Parser [(Column, RecordValue)]
parseDictionary = undefined

alterDataParser :: Parser AlterData
alterDataParser = undefined

createParser :: Parser CommandData
createParser = Create <$> parseList

alterParser :: Parser CommandData
alterParser = Alter <$> choice [
        string "ADD" >> space1 >> addParser,
        string "DROP" >> space1 >> string "COLUMN" >> space1 >> dropParser,
        string "RENAME" >> space1 >> string "COLUMN" >> space1 >> renameParser
    ]
    where
        addParser = Add <$> parseWord
        dropParser = Drop <$> parseWord
        renameParser = Rename <$> parseWord <* space1 <* string "TO" <* space1 <*> parseWord

insertParser :: Parser CommandData
insertParser = Insert <$> parseList <* space1 <* string "VALUES" <* space1 <*> parseList `sepBy1` char ','

updateParser :: Parser CommandData
updateParser = string "SET" *> space1 *> (Update <$> parseDictionary <* space1 <*> whereParser)

deleteParser :: Parser CommandData
deleteParser = undefined

selectParser :: Parser CommandData
selectParser = undefined

unionParser :: Parser CommandData
unionParser = undefined

intersectionParser :: Parser CommandData
intersectionParser = undefined

differenceParser :: Parser CommandData
differenceParser = undefined

commandParser :: Parser Command
commandParser = choice [
                    string "CREATE" >> assembleWith createParser,
                    string "INSERT" >> space1 >> string "INTO" >> assembleWith insertParser,
                    string "UPDATE" >> assembleWith updateParser,
                    string "DELETE" >> assembleWith deleteParser,
                    string "ALTER" >> assembleWith alterParser,
                    string "SELECT" >> assembleWith selectParser,
                    string "UNION" >> assembleWith unionParser,
                    string "INTERSECTION" >> assembleWith intersectionParser,
                    string "DIFFERENCE" >> assembleWith differenceParser
                ] <* space <* char ';'
    where
        assembleWith commandDataParser = space1 *> (Cmd <$> supportedFilePath <* space1 <*> commandDataParser)