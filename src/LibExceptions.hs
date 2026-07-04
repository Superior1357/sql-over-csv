module LibExceptions where
    
import Control.Exception (Exception) 
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void (Void)

data ApplicationException = IOTableException { mess :: String } |
                            ParseException { bundle :: ParseErrorBundle String Void  } deriving (Show, Eq)

instance Exception ApplicationException