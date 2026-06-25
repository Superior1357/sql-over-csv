module Main (main) where
import Test.Hspec
import qualified Parsers (commandParser)


parsersTests :: IO ()
parsersTests = hspec $ do
    describe "Parsers.commandParser" $ do
        it "CREATE command parsed correctly" $ do
            

main :: IO ()
main = do
    parsersTests
