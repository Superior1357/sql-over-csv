module Parsers (Command, commandParser) where
import Commands
import Data.Void (Void)
import Text.Megaparsec (Parsec, some, sepBy1, choice, satisfy)
import Text.Megaparsec.Char (string, space1, char)
import Types (Command (..))
import Control.Applicative (many)
import Control.Monad (void)

type Parser = Parsec Void String

someExcept :: [Char] -> Parser String
someExcept cs = some $ satisfy (`notElem` cs)

manyExcept :: [Char] -> Parser String
manyExcept cs = many $ satisfy (`notElem` cs)

supportedFilePath :: Parser FilePath
supportedFilePath = someExcept [' ']

parseList :: Parser [String]
parseList = char '(' *> manyExcept [',', '(', ')'] `sepBy1` char ',' <* char ')'

createParser :: Parser Command
createParser = do
    space1
    filename <- supportedFilePath
    space1
    Cmd . create filename <$> parseList

alterParser :: Parser Command
alterParser = undefined

insertParser :: Parser Command
insertParser = do
    space1
    filename <- supportedFilePath
    space1
    columns <- parseList
    space1
    void $ string "VALUES"
    space1
    Cmd . insert filename columns <$> parseList

updateParser :: Parser Command
updateParser = undefined

deleteParser :: Parser Command
deleteParser = undefined

selectParser :: Parser Command
selectParser = undefined

unionParser :: Parser Command
unionParser = undefined

intersectionParser :: Parser Command
intersectionParser = undefined

differenceParser :: Parser Command
differenceParser = undefined

commandParser :: Parser Command
commandParser = choice [
                    string "CREATE" >> createParser,
                    string "INSERT" >> space1 >> string "INTO" >> insertParser,
                    string "UPDATE" >> updateParser,
                    string "DELETE" >> deleteParser,
                    string "ALTER" >> alterParser,
                    string "SELECT" >> selectParser,
                    string "UNION" >> unionParser,
                    string "INTERSECTION" >> intersectionParser,
                    string "DIFFERENCE" >> differenceParser
                ]