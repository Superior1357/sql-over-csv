module LibControl (runCommand, openTable) where
import Parsers (commandParser)
import ParsingTypes (Command (..))
import DataTypes (GenericTable (Table), GenericRecord (Record), )

import Text.Megaparsec (runParser)
import Data.Csv (decode, HasHeader (NoHeader))

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL

parseCommand :: String -> Command
parseCommand = undefined

openTable :: FilePath -> IO GenericTable
openTable path = do
    csvText <- BL.readFile path
    let Right v = decode NoHeader csvText :: Either String (Vector (Vector ByteString))
    let table = Table $ V.map Record v
    pure table

saveTable :: GenericTable -> IO ()
saveTable = undefined

runCommand :: String -> IO ()
runCommand c = case runParser commandParser "" c of
    Left err -> putStrLn $ "Invalid command: " ++ show err
    Right command -> print command