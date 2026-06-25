module Parsers (Command, commandParser) where
import Commands
import Data.Void (Void)
import Text.Megaparsec (Parsec, some, sepBy1, choice, satisfy)
import Text.Megaparsec.Char (string, space1, char)
import ParsingTypes (Command (..), CommandData (..))
import Control.Applicative (many)

type Parser = Parsec Void String

someExcept :: [Char] -> Parser String
someExcept cs = some $ satisfy (`notElem` cs)

manyExcept :: [Char] -> Parser String
manyExcept cs = many $ satisfy (`notElem` cs)

supportedFilePath :: Parser FilePath
supportedFilePath = someExcept [' ']

parseList :: Parser [String]
parseList = char '(' *> manyExcept [',', '(', ')'] `sepBy1` char ',' <* char ')'

createParser :: Parser CommandData
createParser = Create <$> parseList <* char ';'

alterParser :: Parser CommandData
alterParser = undefined

insertParser :: Parser CommandData
insertParser = Insert <$> parseList <* space1 <* string "VALUES" <* space1 <*> parseList `sepBy1` char ',' <* char ';'

updateParser :: Parser CommandData
updateParser = undefined

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
                ]
    where
        assembleWith commandDataParser = space1 *> (Cmd <$> supportedFilePath <* space1 <*> commandDataParser)