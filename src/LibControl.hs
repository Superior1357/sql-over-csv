module LibControl (runCommand, openTable) where
import Parsers (commandParser)
import ParsingTypes (Command (..))
import DataTypes (GenericTable)

import Text.Megaparsec (runParser)

parseCommand :: String -> Command
parseCommand = undefined

openTable :: FilePath -> GenericTable
openTable = undefined

saveTable :: GenericTable -> IO ()
saveTable = undefined

runCommand :: String -> IO ()
runCommand c = case runParser commandParser "" c of
    Left err -> putStrLn $ "Invalid command: " ++ show err
    Right command -> print command