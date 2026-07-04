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

data AlterData c = Add { columnName :: c } |
                 Drop { columnName :: c } |
                 Rename { 
                    currentName :: c,
                    newName :: c
                 } deriving (Show, Eq)

data SetOperation = Intersection | Union | Difference deriving (Show, Eq)

data (Show c, Show v, Eq c, Eq v, Show t, Eq t) => IOCommandData c v t = 
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

    Select {
        columns :: [c]
    } deriving (Show, Eq)

newtype (Show c, Eq c) => OutputCommandData c = 
    Create {
        columnNames :: [c]
    } deriving (Eq, Show)

data CommandData c v t = IOCmd (IOCommandData c v t) | OutputCmd (OutputCommandData c) deriving (Show, Eq)

data (Show d, Eq d, Show d2, Eq d2) => Command d d2 =
    OneTableCmd {
        csv_name :: FilePath,
        cmd_data :: d
    } |

    TwoTableCmd {
        t1_name :: FilePath,
        t2_name :: FilePath,
        tr_name :: FilePath,
        command_data :: d2
    } deriving (Show, Eq)