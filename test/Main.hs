{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Test.Hspec
import Parsers ( commandParser, whereParser, parseWord, parseList, parseDictionary)
import LibControl (openTable)

import Data.Vector (singleton, fromList)

import Text.Megaparsec (runParser)
import Commands
import DataTypes
import CommandExceptions
import Control.Exception (evaluate)
import LibExceptions (ApplicationException(IOTableException))

parsersTests :: Spec
parsersTests = do
    describe "Parsers.commandParser" $ do
        it "CREATE command parsed correctly" $ runParser commandParser "" "CREATE example1 (A,B,C);" `shouldBe` Right (OneTableCmd "example1" $ OutputCmd $ Create ["A", "B", "C"])
        it "INSERT command parsed correctly" $ runParser commandParser "" "INSERT INTO example2 (a,b,c) VALUES (1,2,3),(4,5,6);" `shouldBe` Right (OneTableCmd "example2" $ IOCmd $ Insert ["a", "b", "c"] [Record ["1", "2", "3"], Record ["4", "5", "6"]])
        it "ALTER ADD parsed correctly" $ runParser commandParser "" "ALTER example3 ADD c;" `shouldBe` Right (OneTableCmd "example3" $ IOCmd $ Alter (Add "c"))
        it "ALTER DROP parsed correcly" $ runParser commandParser "" "ALTER example4 DROP COLUMN c2;" `shouldBe` Right (OneTableCmd "example4" $ IOCmd $ Alter (Drop "c2"))
        it "ALTER RENAME parsed correctly" $ runParser commandParser "" "ALTER example5 RENAME COLUMN old TO new;" `shouldBe` Right (OneTableCmd "example5" $ IOCmd $ Alter (Rename "old" "new"))
        it "SELECT command parsed correclty" $ runParser commandParser "" "SELECT (col1,col2) FROM example6;" `shouldBe` Right (OneTableCmd "example6" $ IOCmd $ Select ["col1", "col2"])
        it "UPDATE command parsed correctly (without WHERE)" $ runParser commandParser "" "UPDATE table SET (c1 = v1,c2 = v2);" `shouldBe` Right (OneTableCmd "table" $ IOCmd (Update [("c1", "v1"), ("c2", "v2")] NoCondition))
        it "UPDATE command parsed correctly (with WHERE)" $ runParser commandParser "" "UPDATE table SET (c1 = 3,c2 = 9) WHERE c1 > 3;" `shouldBe` Right (OneTableCmd "table" $ IOCmd (Update [("c1", "3"), ("c2", "9")] (Greater "c1" "3")))
        it "DELETE command parsed correctly" $ runParser commandParser "" "DELETE FROM table WHERE col = \"Hello world\";" `shouldBe` Right (OneTableCmd "table" $ IOCmd (Delete (Equal "col" "Hello world")))

        it "UNION command parsed correctly" $ runParser commandParser "" "UNION t1, t2 INTO t3;" `shouldBe` Right (TwoTableCmd "t1" "t2" "t3" Union)
        it "INTERSECTION command parsed correctly" $ runParser commandParser "" "INTERSECTION t1, t2 INTO t3;" `shouldBe` Right (TwoTableCmd "t1" "t2" "t3" Intersection)
        it "DIFFERENCE command parsed correctly" $ runParser commandParser "" "DIFFERENCE t1, t2 INTO t3;" `shouldBe` Right (TwoTableCmd "t1" "t2" "t3" Difference)

        it "WHERE > parsed correctly" $ runParser whereParser "" "WHERE c1 > 45" `shouldBe` Right (Greater "c1" "45")
        it "WHERE < parsed correctly" $ runParser whereParser "" "WHERE c1 < 45" `shouldBe` Right (Less "c1" "45")
        it "WHERE = parsed correctly" $ runParser whereParser "" "WHERE c1 = 45" `shouldBe` Right (Equal "c1" "45")
        it "WHERE <> parsed correctly" $ runParser whereParser "" "WHERE c1 <> 45" `shouldBe` Right (NotEqual "c1" "45")
        it "WHERE >= parsed correctly" $ runParser whereParser "" "WHERE c1 >= 45" `shouldBe` Right (GreaterEqual "c1" "45")
        it "WHERE <= parsed correctly" $ runParser whereParser "" "WHERE c1 <= 45" `shouldBe` Right (LessEqual "c1" "45")
        it "WHERE IN parsed correctly" $ runParser whereParser "" "WHERE c1 IN (45,25,34)" `shouldBe` Right (In "c1" ["45", "25", "34"])

    describe "Parsers.parseWord" $ do
        it "without double quotes parsed" $ runParser parseWord "" "hello" `shouldBe` Right "hello"
        it "with double quotes parsed" $ runParser parseWord "" "\"hel\"\"l;  o\"" `shouldBe` Right "hel\"l;  o"

    describe "Parsers.parseList" $ do
        it "without spaces" $ runParser parseList "" "(1,2,3)" `shouldBe` Right ["1", "2", "3"]
        it "with whitespace" $ runParser parseList "" "( 1,  2,3)" `shouldBe` Right ["1", "2", "3"]
        it "with whitespace and tabulators" $ runParser parseList "" "( 1,  2,         3)" `shouldBe` Right ["1", "2", "3"]

        it "empty list" $ runParser parseList "" "()" `shouldBe` Right []
        it "empty list - whitespace" $ runParser parseList "" "(  )" `shouldBe` Right []

    describe "Parsers.parseDictionary" $ do
        it "without spaces" $ runParser parseDictionary "" "(c1=v1,c2=v2)" `shouldBe` Right [("c1", "v1"), ("c2", "v2")]
        it "with whitespace" $ runParser parseDictionary "" "( c1= v1 , c2  =v2)" `shouldBe` Right [("c1", "v1"), ("c2", "v2")]
        it "with whitespace and tabulators" $ runParser parseDictionary "" "( c1= v1 , c2  =      v2      )" `shouldBe` Right [("c1", "v1"), ("c2", "v2")]
        it "with complex word" $ runParser parseDictionary "" "( \"a = b\"= v1 , c2  =v2)" `shouldBe` Right [("a = b", "v1"), ("c2", "v2")]

force :: (Show a) => a -> Int 
force = length.show

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

rowNumbersOnly :: RecordType
rowNumbersOnly = Record $ fromList ["11", "22", "33"]

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

commandsTests :: Spec
commandsTests = do
    describe "Commands.applyCommand - valid input tests" $ do
        it "INSERT command applied correctly" $ do
            let cmd = Insert ["AAA", "C"] [Record ["EE", "II"], Record ["OO", "LL"]]
            applyCommand exampleTable1 cmd `shouldBe` exampleTable2

        it "INSERT command applied correctly - column order different from header" $ do
            let cmd = Insert ["C", "AAA"] [Record ["II", "EE"], Record ["LL", "OO"]]
            applyCommand exampleTable1 cmd `shouldBe` exampleTable2

        it "UPDATE command applied correctly" $ do
            let cmd = Update [("AAA", "gg"), ("BB", "12")] (Equal "BB" "67")
            applyCommand exampleTable1 cmd `shouldBe` Table (fromList [tableHeader, row1, row5])

        it "UPDATE command applied correctly - column order different from header" $ do
            let cmd = Update [("BB", "12"), ("AAA", "gg")] (Equal "BB" "67")
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

        it "SELECT command applied correctly - column order different from header" $ do
            let cmd = Select ["C", "BB"]
            let shortHeader = Record $ fromList ["C", "BB"]
            let selectedRow1 = Record $ fromList ["$@#i", "3"]
            let selectedRow2 = Record $ fromList [",ee", "67"]

            applyCommand exampleTable1WithNewC cmd `shouldBe` Table (fromList [shortHeader, selectedRow1, selectedRow2])

        it "INTERSECTION command applied correctly" $ do
            let t1 = Table $ fromList [tableHeader, row1, row2, row3]
            let t2 = Table $ fromList [tableHeader, row1, row3, row4]
            let t3 = Table $ fromList [tableHeader, row1, row3]

            applyTwoTableCommand t1 t2 Intersection `shouldBe` t3

        it "UNION command applied correctly" $ do
            let t1 = Table $ fromList [tableHeader, row1, row2, row3]
            let t2 = Table $ fromList [tableHeader, row1, row3, row4]
            let t3 = Table $ fromList [tableHeader, row1, row2, row3, row4]

            applyTwoTableCommand t1 t2 Union `shouldBe` t3

        it "DIFFERENCE command applied correctly" $ do
            let t1 = Table $ fromList [tableHeader, row1, row2, row3]
            let t2 = Table $ fromList [tableHeader, row1, row3, row4]
            let t3 = Table $ fromList [tableHeader, row2]

            applyTwoTableCommand t1 t2 Difference `shouldBe` t3

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

    describe "Commands.applyOutputCommand -> valid input test" $ do
        it "CREATE command applied correctly" $ do
            let cmd = Create ["AAA", "BB", "C"]
            applyOutputCommand cmd `shouldBe` onlyHeaderTable

    describe "Commands.applyCommand - exceptions tests" $ do
        it "Commands.applyCommand -> InvalidTableFormatExceptionThrown if a record is longer than the header" $ do
            let wrongTable = Table $ fromList [tableHeader, row1, row2Extended, row1]
            evaluate (force (applyCommand wrongTable (Delete NoCondition))) `shouldThrow` (== InvalidTableFormatException "2")
        it "Commands.applyCommand -> InvalidTableFormatExceptionThrown if a record is shorter than the header" $ do
            let wrongTable = Table $ fromList [tableHeader, row1, row2Shortened, row1]
            evaluate (force (applyCommand wrongTable (Delete NoCondition))) `shouldThrow` (== InvalidTableFormatException "2")
        it "Commands.applyCommand -> InvalidTableFormatExceptionThrown if the header empty" $ do -- TODO: does it make any sense
            let wrongTable = Table $ fromList [Record (fromList []), row1, row1, row1]
            evaluate (force (applyCommand wrongTable (Delete NoCondition))) `shouldThrow` (== InvalidTableFormatException "0")
        it "Commands.applyCommand -> InvalidTableFormatExceptionThrown if the header contains duplicit column names" $ do
            let wrongTable = Table $ fromList [Record (fromList ["AAA", "BB", "AAA"]), row1, row1, row1]
            evaluate (force (applyCommand wrongTable (Delete NoCondition))) `shouldThrow` (== InvalidTableFormatException "0")


        it "Commands.correspondingIndex -> ColumnNotFoundException thrown if column index not found" $ evaluate (force (force (correspondingIndex tableHeader "NOT PRESENT"))) `shouldThrow` (== ColumnNotFoundException "NOT PRESENT")
        it "Command.fIntInterpreted -> UnableToInterpretException thrown if unable to intepret a field as an Int" $ evaluate (force (fIntInterpreted "33" (\a b -> a == 3 || b == 4) "4hello")) `shouldThrow` (== UnableToInterpretException "4hello")
        
        it "CREATE command duplicate column name -- ColumnNameDuplicatedException thrown" $ evaluate (force (create ["AAA", "BB", "AAA"])) `shouldThrow` (== ColumnNameDuplicatedException "AAA")
        it "CREATE command empty column list - InvalidArgCountException thrown" $ evaluate (force (create [])) `shouldThrow` (== InvalidArgCountException "0")

        it "INSERT command invalid column name - ColumnNotFoundException thrown" $ evaluate (force (insert exampleTable1 ["AAA", "nonsense"] [Record ["11", "22"], Record ["AA", "AA"]])) `shouldThrow` (== ColumnNotFoundException "nonsense")
        it "INSERT command invalid inserted VALUES item length - InvalidArgCountException thrown" $ evaluate (force (insert exampleTable1 ["AAA", "BB"] [Record ["11", "22"], Record ["11", "22", "33"]])) `shouldThrow` (== InvalidArgCountException "3")
        it "INSERT command duplicate column name -- ColumnNameDuplicatedException thrown" $ evaluate (force (insert exampleTable1 ["AAA", "BB", "AAA"] [Record ["11", "22", "33"], Record ["AA", "AA", "AA"]])) `shouldThrow` (== ColumnNameDuplicatedException "AAA")

        it "UPDATE command invalid column name - ColumnNotFoundException thrown" $ evaluate (force (update exampleTable1 [("AAA", "1"), ("nonsense", "2")] NoCondition)) `shouldThrow` (== ColumnNotFoundException "nonsense")
        it "UPDATE command duplicate column name -- ColumnNameDuplicatedException thrown" $ evaluate (force (update exampleTable1 [("AAA", "1"), ("AAA", "3"), ("BB", "2")] NoCondition)) `shouldThrow` (== ColumnNameDuplicatedException "AAA")

        it "ALTER DROP command invalid column name - ColumnNotFoundException thrown" $ evaluate (force (alterDrop exampleTable1 "nonsense")) `shouldThrow` (== ColumnNotFoundException "nonsense")
        
        it "ALTER RENAME command invalid column name - ColumnNotFoundException thrown" $ evaluate (force (alterRename exampleTable1 "nonsense" "not necessary")) `shouldThrow` (== ColumnNotFoundException "nonsense")
        it "ALTER RENAME trying to rename to an existing column name - ColumnNameDuplicatedException thrown" $ evaluate (force (alterRename exampleTable1 "AAA" "BB")) `shouldThrow` (== ColumnNameDuplicatedException "BB")

        it "ALTER ADD command trying to add an existing column - ColumnNameDuplicatedException thrown" $ evaluate (force (alterAdd exampleTable1 "BB")) `shouldThrow` (== ColumnNameDuplicatedException "BB")

        it "SELECT command invalid column name - ColumnNotFoundException thrown" $ evaluate (force (select exampleTable1 ["AAA", "nonsense"]))  `shouldThrow` (== ColumnNotFoundException "nonsense")
        it "SELECT command duplicate column name -- ColumnNameDuplicatedException thrown" $ evaluate (force (select exampleTable1 ["AAA", "BB", "AAA"])) `shouldThrow` (== ColumnNameDuplicatedException "AAA")

        it "UNION command tables headers differ - HeaderDifferException thrown" $ evaluate (force (applySetOperationCommand exampleTable1 exampleTable1WithNewC Union)) `shouldThrow` (\m -> m == HeadersDifferException "" || m == HeadersDifferException "NewC")
        it "INTERSECTION command tables headers differ - HeaderDifferException thrown" $ evaluate (force (applySetOperationCommand exampleTable1 exampleTable1WithNewC Intersection)) `shouldThrow` (\m -> m == HeadersDifferException "" || m == HeadersDifferException "NewC")
        it "DIFFERENCE command tables headers differ - HeaderDifferException thrown" $ evaluate (force (applySetOperationCommand exampleTable1 exampleTable1WithNewC Difference)) `shouldThrow` (\m -> m == HeadersDifferException "" || m == HeadersDifferException "NewC")

        it "whereFunc -> ColumnNotFoundException thrown if column from WHERE clause not present" $ evaluate (force (whereFunc tableHeader "nonsense" (== "") rowNumbersOnly)) `shouldThrow` (== ColumnNotFoundException "nonsense")
        it "interpretWhereCondition > -> UnableToInterpretException thrown if unable to intepret argument as an Int" $ evaluate (force (interpretWhereCondition (Greater "AAA" "1H") tableHeader rowNumbersOnly)) `shouldThrow` (== UnableToInterpretException "1H")
        it "interpretWhereCondition < -> UnableToInterpretException thrown if unable to intepret argument as an Int" $ evaluate (force (interpretWhereCondition (Less "AAA" "1H") tableHeader rowNumbersOnly)) `shouldThrow` (== UnableToInterpretException "1H")
        it "interpretWhereCondition >= -> UnableToInterpretException thrown if unable to intepret argument as an Int" $ evaluate (force (interpretWhereCondition (GreaterEqual "AAA" "1H") tableHeader rowNumbersOnly)) `shouldThrow` (== UnableToInterpretException "1H")
        it "interpretWhereCondition <= -> UnableToInterpretException thrown if unable to intepret argument as an Int" $ evaluate (force (interpretWhereCondition (LessEqual "AAA" "1H") tableHeader rowNumbersOnly)) `shouldThrow` (== UnableToInterpretException "1H")

    describe "Commands.applyOutputCommand -> exceptions tests" $ do
        it "CREATE command - empty columns list throws an exception" $ do
            let cmd = Create []
            evaluate (force (applyOutputCommand cmd)) `shouldThrow` (== InvalidArgCountException "0")

inputTests :: Spec
inputTests = describe "LibControl.openTable"  $ do
                it "openTable opens when file valid" $ do
                    table <- openTable "test/example1.csv"
                    table `shouldBe` exampleTable1

                it "openTable throws an exception when unable to open file" $ do
                    openTable "nonexistent" `shouldThrow` (== IOTableException "nonexistent")

main :: IO ()
main = hspec $ do
    parsersTests
    commandsTests
    inputTests

    -- TODO: two table commands should have their respective checks too - maybe should be checked upon opening, not later