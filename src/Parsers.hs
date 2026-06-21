module Parsers (Command, commandParser) where
import Commands
import Data.Void (Void)
import Text.Megaparsec (Parsec, some, sepBy1, choice)
import Text.Megaparsec.Char (string, space1, alphaNumChar, char)
import Types (Command (..))

type Parser = Parsec Void String

createParser :: Parser Command
createParser = do
    _ <- space1
    filename <- some alphaNumChar
    space1
    columns <- some alphaNumChar `sepBy1` char ','
    pure $ Cmd $ create filename columns

alterParser :: Parser Command
alterParser = undefined

insertParser :: Parser Command
insertParser = undefined

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
                    string "INSERT" >> insertParser,
                    string "UPDATE" >> updateParser,
                    string "DELETE" >> deleteParser,
                    string "ALTER" >> alterParser,
                    string "SELECT" >> selectParser,
                    string "UNION" >> unionParser,
                    string "INTERSECTION" >> intersectionParser,
                    string "DIFFERENCE" >> differenceParser
                ]