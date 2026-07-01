module DataTypes where

newtype Record vs = Record vs deriving (Show, Eq)
newtype Table rs = Table rs deriving (Show, Eq)

data WhereCondition c v = Equal c v
                     |
                      Greater c v
                     |
                      Less c v
                     |
                      GreaterEqual c v
                     |
                      LessEqual c v
                     |
                      NotEqual c v
                     |
                      In c [v]
                     |
                      NoCondition deriving (Show, Eq)

data CsvType = CSVInt (Maybe Int) | CSVString (Maybe String)

data AlterData c = Add { columnName :: c } |
                 Drop { columnName :: c } |
                 Rename { 
                    currentName :: c,
                    newName :: c
                 } deriving (Show, Eq)

data SetOperation = Intersection | Union | Difference deriving (Show, Eq)

data (Show c, Show v, Eq c, Eq v, Show t, Eq t) => CommandData c v t = 
    Create {
        columnNames :: [c]
    } |

    Insert {
        columns :: [c],
        records :: [Record [v]]
    } |

    Update {
        valueUpdates :: [(c, v)],
        condition :: WhereCondition c v
    } |

    Delete {
        condition :: WhereCondition c v
    } |

    Alter {
        subCommand :: AlterData c
    } |

    SetOperation {
        secondTable :: t,
        resultTable :: t,
        operation :: SetOperation
    } |

        Select {
        columns :: [c]
    } deriving (Show, Eq)

data (Show d, Eq d) => Command d = Cmd {
    csv_name :: FilePath,
    cmd_data :: d
} deriving (Show, Eq)