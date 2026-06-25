{-#LANGUAGE FlexibleContexts#-}
module Commands where
import Data.List (intercalate)

{-
import qualified DataFrame as D
import qualified Data.Text as T

create :: FilePath -> [String] -> IO ()
create path columns = writeFile path $ intercalate "," columns ++ "\n"

insert :: FilePath -> [String] -> [String] -> IO ()
insert path columns values = do
    csv <- D.readCsv path
    D.writeCsv path $ D.fold go (zip columns values) csv
    where
      go (column, value) = D.insert (T.pack column) [value]

-- update :: FilePath -> [(String, String)] -> []
-}