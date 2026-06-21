module Commands where
import Data.List (intercalate)

create :: String -> [String] -> IO ()
create fileName columns = writeFile fileName $ intercalate "," columns