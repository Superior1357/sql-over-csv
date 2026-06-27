module ParsingTypes where

type RecordValue = String
type Column = String

data WhereCondition = Equal Column RecordValue |
                      Greater Column RecordValue
                     |
                      Less Column RecordValue
                     |
                      GreaterEqual Column RecordValue
                     |
                      LessEqual Column RecordValue
                     |
                      NotEqual Column RecordValue
                     |
                      Between Column [RecordValue
                    ] |
                      In Column [RecordValue
                    ] |
                      NoCondition deriving (Show, Eq)

data CsvType = CSVInt (Maybe Int) | CSVString (Maybe String)

data AlterData = Add { columnName :: Column } |
                 Drop { columnName :: Column } |
                 Rename { 
                    currentName :: Column,
                    newName :: Column
                 } deriving (Show, Eq)

data SetOperation = Intersection | Union | Difference deriving (Show, Eq)

data CommandData = 
    Create {
        columnNames :: [Column]
    } |

    Insert {
        columns :: [Column],
        records :: [[RecordValue]]
    } |

    Update {
        valueUpdates :: [(Column, RecordValue)],
        condition :: WhereCondition
    } |

    Delete {
        condition :: WhereCondition
    } |

    Alter {
        subCommand :: AlterData
    } |

    SetOperation {
        secondTable :: FilePath,
        resultTable :: FilePath,
        operation :: SetOperation
    } |

        Select {
        columns :: [Column]
    } deriving (Show, Eq)

data Command = Cmd {
    csv_name :: FilePath,
    cmd_data :: CommandData
} deriving (Show, Eq)