module Main (main) where
import Test.Hspec
import qualified Parsers (commandParser)
import Text.Megaparsec (runParser)

import Parsers (commandParser)
import Types (Command (Cmd), CommandData (Create, Insert))

parsersTests :: IO ()
parsersTests = hspec $ do
    describe "Parsers.commandParser" $ do
        it "CREATE command parsed correctly" $ do
            runParser commandParser "" "CREATE example1 (A,B,C);" `shouldBe` Right (Cmd "example1" (Create ["A", "B", "C"]))
        it "INSERT command parsed correctly" $ do
            runParser commandParser "" "INSERT INTO example2 (a,b,c) VALUES (1,2,3),(4,5,6);" `shouldBe` Right (Cmd "example2" (Insert ["a", "b", "c"] [["1", "2", "3"], ["4", "5", "6"]] ))
        it "ALTER ADD parsed correctly" $ do
            runParser commandParser "" ""


main :: IO ()
main = do
    parsersTests
