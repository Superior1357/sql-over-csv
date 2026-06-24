module LibControl (runCommand) where
import Parsers (commandParser)
import Types (Command (..))
import Text.Megaparsec (runParser)

runCommand :: String -> IO ()
runCommand c = case runParser commandParser "" c of
    Left err -> putStrLn $ "Invalid command: " ++ show err
    Right command -> print command