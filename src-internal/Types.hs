module Types where

type RecordType = String
type ColumnType = String
type WhereCondition = ( CsvType -> Bool )

data CsvType = CSVInt (Maybe Int) | CSVString (Maybe String) | CSVBool (Maybe Bool)

data AlterData = Add { columnName :: ColumnType } |
                 Drop { columnName :: ColumnType } |
                 Rename { 
                    currentName :: ColumnType,
                    newName :: ColumnType
                 } deriving (Show, Eq)

data SetOperation = Intersection | Union | Difference deriving (Show, Eq)

data CommandData = 
    Create {
        columnNames :: [ColumnType]
    } |

    Insert {
        columns :: [ColumnType],
        records :: [[RecordType]]
    } |

    Update {
        columnsToUpdate :: [(ColumnType, RecordType)]
        -- condition :: WhereCondition       
    } |

    Delete {
        -- condition :: WhereCondition
    } |

    Alter {
        subCommand :: AlterData
    } |

    SetOperation {
        secondTable :: FilePath,
        resultTable :: FilePath,
        operation :: SetOperation
    } deriving (Show, Eq)

data Command = Cmd {
    csv_name :: FilePath,
    cmd_data :: CommandData
} deriving (Show, Eq)

class Stream a where
    write :: a -> String -> IO ()
    read :: a -> IO String