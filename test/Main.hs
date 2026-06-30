{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Test.Hspec
import Parsers ( commandParser, whereParser)
import LibControl (openTable)

import Data.Vector (singleton, fromList)

import Text.Megaparsec (runParser)
import Commands ( applyCommand, emptyTable, CommandTable, RecordType, interpretWhereCondition)
import DataTypes

-- TODO: implement double quoted values
-- TODO: test all where conditions
parsersTests :: Spec
parsersTests = do
    describe "Parsers.commandParser" $ do
        it "CREATE command parsed correctly" $ do
            runParser commandParser "" "CREATE example1 (A,B,C);" `shouldBe` Right (Cmd "example1" (Create ["A", "B", "C"]))
        it "INSERT command parsed correctly" $ do
            runParser commandParser "" "INSERT INTO example2 (a,b,c) VALUES (1,2,3),(4,5,6);" `shouldBe` Right (Cmd "example2" (Insert ["a", "b", "c"] [Record ["1", "2", "3"], Record ["4", "5", "6"]] ))
        it "ALTER ADD parsed correctly" $ do
            runParser commandParser "" "ALTER example3 ADD c;" `shouldBe` Right (Cmd "example3" (Alter (Add "c")))
        it "ALTER DROP parsed correcly" $ do
            runParser commandParser "" "ALTER example4 DROP COLUMN c2;" `shouldBe` Right (Cmd "example4" (Alter (Drop "c2")))
        it "ALTER RENAME parsed correctly" $ do
            runParser commandParser "" "ALTER example5 RENAME COLUMN old TO new;" `shouldBe` Right (Cmd "example5" (Alter (Rename "old" "new")))
        it "SELECT command parsed correclty" $ do
            runParser commandParser "" "SELECT (col1,col2) FROM example6;" `shouldBe` Right (Cmd "example6" (Select ["col1", "col2"]))
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

        it "WHERE > parsed correctly" $ do
            runParser whereParser "" "WHERE c1 > 45" `shouldBe` Right (Greater "c1" "45")
        it "WHERE < parsed correctly" $ do
            runParser whereParser "" "WHERE c1 < 45" `shouldBe` Right (Less "c1" "45")
        it "WHERE = parsed correctly" $ do
            runParser whereParser "" "WHERE c1 = 45" `shouldBe` Right (Equal "c1" "45")
        it "WHERE <> parsed correctly" $ do
            runParser whereParser "" "WHERE c1 <> 45" `shouldBe` Right (NotEqual "c1" "45")
        it "WHERE >= parsed correctly" $ do
            runParser whereParser "" "WHERE c1 >= 45" `shouldBe` Right (GreaterEqual "c1" "45")
        it "WHERE <= parsed correctly" $ do
            runParser whereParser "" "WHERE c1 <= 45" `shouldBe` Right (LessEqual "c1" "45")
        it "WHERE IN parsed correctly" $ do
            runParser whereParser "" "WHERE c1 IN (45,25,34)" `shouldBe` Right (In "c1" ["45", "25", "34"])

tableHeader :: RecordType
tableHeader = Record $ fromList ["AAA", "BB", "C"]

tableHeader2 :: RecordType
tableHeader2 = Record $ fromList ["AAA", "BB", "C", "NewC"]

onlyHeaderTable :: CommandTable
onlyHeaderTable = Table (singleton tableHeader)

row1 :: RecordType
row1 = Record $ fromList ["Item1", "3", "$@#i"]

row1Extended :: RecordType
row1Extended = Record $ fromList ["Item1", "3", "$@#i", ""]

row1Shortened :: RecordType
row1Shortened = Record $ fromList ["3", "$@#i"]

row2 :: RecordType
row2 = Record $ fromList ["\"13\"", "67", ",ee"]

row2Extended :: RecordType
row2Extended = Record $ fromList ["\"13\"", "67", ",ee", ""]

row2Shortened :: RecordType
row2Shortened = Record $ fromList ["67", ",ee"]

row3 :: RecordType
row3 = Record $ fromList ["EE", "", "II"]

row4 :: RecordType
row4 = Record $ fromList ["OO", "", "LL"]

row5 :: RecordType
row5 = Record $ fromList ["gg", "12", ",ee"]

exampleTable1 :: CommandTable
exampleTable1 = Table $ fromList [tableHeader, row1, row2]

exampleTable1WithNewC :: CommandTable
exampleTable1WithNewC = Table $ fromList [tableHeader2, row1Extended, row2Extended]

exampleTable2 :: CommandTable
exampleTable2 = Table $ fromList [tableHeader, row1, row2, row3, row4]

-- TODO: test all WHERE clauses
commandsTests :: Spec
commandsTests = do
    describe "DataTypes.applyCommand" $ do
        it "CREATE command applied correctly" $ do
            let cmd = Create ["AAA", "BB", "C"]
            applyCommand emptyTable cmd `shouldBe` onlyHeaderTable

        it "INSERT command applied correctly" $ do
            let cmd = Insert ["AAA", "C"] [Record ["EE", "II"], Record ["OO", "LL"]]
            applyCommand exampleTable1 cmd `shouldBe` exampleTable2

        it "UPDATE command applied correctly" $ do
            let cmd = Update [("AAA", "gg"), ("BB", "12")] (Equal "BB" "67")
            applyCommand exampleTable1 cmd `shouldBe` Table (fromList [tableHeader, row1, row5])

        it "DELETE command applied correctly" $ do
            let cmd = Delete (Equal "BB" "")
            applyCommand exampleTable2 cmd `shouldBe` exampleTable1

        it "ALTER ADD command applied correclty" $ do
            let cmd = Alter $ Add "NewC"
            applyCommand exampleTable1 cmd `shouldBe` exampleTable1WithNewC

        it "ALTER DROP command applied correctly" $ do
            let cmd = Alter $ Drop "NewC"
            applyCommand exampleTable1WithNewC cmd `shouldBe` exampleTable1

        it "ALTER RENAME command applied correctly" $ do
            let cmd = Alter $ Rename "AAA" "AAA2"
            let renamedHeader = Record $ fromList ["AAA2", "BB", "C"]
            applyCommand exampleTable1 cmd `shouldBe` Table (fromList [renamedHeader, row1, row2])

        it "SELECT command applied correctly" $ do
            let cmd = Select ["BB", "C"]
            let shortHeader = Record $ fromList ["BB", "C"]
            applyCommand exampleTable1WithNewC cmd `shouldBe` Table (fromList [shortHeader, row1Shortened, row2Shortened])



        it "WHERE Equal True where condition holds" $ do
            let func = interpretWhereCondition (Equal "AAA" "Item1") tableHeader
            func row1 `shouldBe` True

        it "WHERE Equal False where condition doesn't hold" $ do
            let func = interpretWhereCondition (Equal "AAA" "Item1") tableHeader
            func row2 `shouldBe` False
        
        it "WHERE Greater True where condition holds" $ do
            let func = interpretWhereCondition (Greater "BB" "4") tableHeader
            func row2 `shouldBe` True

        it "WHERE Greater False where condition doesn't hold" $ do
            let func = interpretWhereCondition (Greater "BB" "4") tableHeader
            func row1 `shouldBe` False

        it "WHERE Greater Equal True where condition holds" $ do
            let func = interpretWhereCondition (GreaterEqual "BB" "67") tableHeader
            func row2 `shouldBe` True

        it "WHERE Greater Equal False where condition doesn't hold" $ do
            let func = interpretWhereCondition (GreaterEqual "BB" "67") tableHeader
            func row1 `shouldBe` False

        it "WHERE Less True where condition holds" $ do
            let func = interpretWhereCondition (Less "BB" "4") tableHeader
            func row1 `shouldBe` True

        it "WHERE Less False where condition doesn't hold" $ do
            let func = interpretWhereCondition (Less "BB" "4") tableHeader
            func row2 `shouldBe` False

        it "WHERE Less Equal True where condition holds" $ do
            let func = interpretWhereCondition (LessEqual "BB" "3") tableHeader
            func row1 `shouldBe` True

        it "WHERE Less Equal False where condition doesn't hold" $ do
            let func = interpretWhereCondition (LessEqual "BB" "3") tableHeader
            func row2 `shouldBe` False

        it "WHERE Not Equal True where condition holds" $ do
            let func = interpretWhereCondition (NotEqual "AAA" "Item1") tableHeader
            func row2 `shouldBe` True

        it "WHERE Not Equal False where condition doesn't hold" $ do
            let func = interpretWhereCondition (NotEqual "AAA" "Item1") tableHeader
            func row1 `shouldBe` False

        it "WHERE In True where condition holds" $ do
            let func = interpretWhereCondition (In "AAA" ["Item1", "Item2"]) tableHeader
            func row1 `shouldBe` True

        it "WHERE In False where condition doesn't hold" $ do
            let func = interpretWhereCondition (In "AAA" ["Item1", "Item2"]) tableHeader
            func row2 `shouldBe` False

        it "WHERE NoCondition True test1" $ do
            let func = interpretWhereCondition NoCondition tableHeader
            func row1 `shouldBe` True

        it "WHERE NoCondition True test2" $ do
            let func = interpretWhereCondition NoCondition tableHeader
            func row2 `shouldBe` True

        -- TODO: make set operations better (and make tests for them)

inputTests :: Spec
inputTests = do
    describe "LibControl.openTable" $ do
        it "openTable works" $ do
            table <- openTable "test/example1.csv"
            table `shouldBe` exampleTable1

main :: IO ()
main = hspec $ do
    parsersTests
    commandsTests
    inputTests