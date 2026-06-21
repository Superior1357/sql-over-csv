module Parsers (runCommand, Command) where
import Data.Void (Void)
import Text.Megaparsec (Parsec, some, sepBy1, runParser, (<|>))
import Text.Megaparsec.Char (string, space1, alphaNumChar, char)
import Data.List (intercalate)

type Parser = Parsec Void String
newtype Command = Cmd (IO ())

create :: String -> [String] -> IO ()
create fileName columns = writeFile fileName $ intercalate "," columns

createParser :: Parser Command
createParser = do
    _ <- space1
    filename <- some alphaNumChar
    space1
    columns <- some alphaNumChar `sepBy1` char ','
    pure $ Cmd $ create filename columns

alterParser :: Parser Command
alterParser = undefined

commandParser :: Parser Command
commandParser = parseCreate <|> parseAlter
    where
        parseCreate = string "CREATE" >> createParser
        parseAlter = string "ALTER" >> alterParser

runCommand :: String -> IO ()
runCommand c = case runParser commandParser "" c of
    Left err -> putStrLn $ "Invalid command: " ++ show err
    Right (Cmd commandM) -> commandM