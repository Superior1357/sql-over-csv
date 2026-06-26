{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Test.Hspec
import Parsers ( commandParser )

import Data.Vector (Vector, singleton, fromList)
import Data.ByteString (ByteString)

import Text.Megaparsec (runParser)
import ParsingTypes (Command (Cmd), CommandData (Create, Insert, Alter, Select, SetOperation, Update, Delete), AlterData (Add, Drop, Rename), SetOperation (Union, Intersection, Difference),
                WhereCondition (Equal, Greater, NoCondition))
import DataTypes
import Data.Vector.Generic (generate)

-- TODO: implement double quoted values
-- TODO: test all where conditions
parsersTests :: IO ()
parsersTests = hspec $ do
    describe "Parsers.commandParser" $ do
        it "CREATE command parsed correctly" $ do
            runParser commandParser "" "CREATE example1 (A,B,C);" `shouldBe` Right (Cmd "example1" (Create ["A", "B", "C"]))
        it "INSERT command parsed correctly" $ do
            runParser commandParser "" "INSERT INTO example2 (a,b,c) VALUES (1,2,3),(4,5,6);" `shouldBe` Right (Cmd "example2" (Insert ["a", "b", "c"] [["1", "2", "3"], ["4", "5", "6"]] ))
        it "ALTER ADD parsed correctly" $ do
            runParser commandParser "" "ALTER example3 ADD c;" `shouldBe` Right (Cmd "example3" (Alter (Add "c")))
        it "ALTER DROP parsed correcly" $ do
            runParser commandParser "" "ALTER example4 DROP COLUMN c2;" `shouldBe` Right (Cmd "example4" (Alter (Drop "c2")))
        it "ALTER RENAME parsed correctly" $ do
            runParser commandParser "" "ALTER example5 RENAME COLUMN old TO new;" `shouldBe` Right (Cmd "example5" (Alter (Rename "old" "new")))
        it "SELECT command parsed correclty" $ do
            runParser commandParser "" "SELECT col1, col2 FROM example6;" `shouldBe` Right (Cmd "example6" (Select ["col1", "col2"]))
        it "UPDATE command parsed correctly (without WHERE)" $ do
            runParser commandParser "" "UPDATE table SET c1 = v1, c2 = v2;" `shouldBe` Right (Cmd "table" (Update [("c1", "v1"), ("c2", "v2")] NoCondition))
        it "UPDATE command parsed correctly (with WHERE)" $ do
            runParser commandParser "" "UPDATE table SET c1 = 3, c2 = 9 WHERE c1 > 3;" `shouldBe` Right (Cmd "table" (Update [("c1", "3"), ("c2", "9")] (Greater "c1" "3")))
        it "DELETE command parsed correctly" $ do
            runParser commandParser "" "DELETE FROM table WHERE col = \"Hello world\";" `shouldBe` Right (Cmd "table" (Delete (Equal "col" "Hello world")))
        it "UNION command parsed correctly" $ do
            runParser commandParser "" "UNION t1, t2 INTO t3;" `shouldBe` Right (Cmd "t1" (SetOperation "t2" "t3" Union))
        it "INTERSECTION command parsed correctly" $ do
            runParser commandParser "" "INTERSECTION t1, t2 INTO t3;" `shouldBe` Right (Cmd "t1" (SetOperation "t2" "t3" Intersection))
        it "DIFFERENCE command parsed correctly" $ do
            runParser commandParser "" "DIFFERENCE t1, t2 INTO t3;" `shouldBe` Right (Cmd "t1" (SetOperation "t2" "t3" Difference))

tableHeader :: GenericRecord
tableHeader = Record $ fromList ["AAA", "BB", "C"]

onlyHeaderTable :: GenericTable
onlyHeaderTable = Table (singleton tableHeader)

row1 :: GenericRecord
row1 = Record $ fromList ["Item1", "3", "$@#i"]

row2 :: GenericRecord
row2 = Record $ fromList ["\"13\"", "067", "\",ee\""]

row3 :: GenericRecord
row3 = Record $ fromList ["EE", "", "II"]

row4 :: GenericRecord
row4 = Record $ fromList ["OO", "", "LL"]

exampleTable1 :: GenericTable
exampleTable1 = Table $ fromList [tableHeader, row1, row2]

exampleTable2 :: GenericTable
exampleTable2 = Table $ fromList [tableHeader, row1, row2, row3, row4]

commandsTests :: IO ()
commandsTests = undefined hspec $ do
    describe "DataTypes.applyCommand" $ do
        it "CREATE command applied correctly" $ do
            let cmd = Create ["AAA", "BB", "C"]
            applyCommand emptyTable cmd `shouldBe` onlyHeaderTable

        it "INSERT command applied correctly" $ do
            let cmd = Insert ["AAA", "C"] [["EE", "II"], ["OO", "LL"]]
            applyCommand exampleTable1 cmd `shouldBe` exampleTable2

main :: IO ()
main = do
    parsersTests
