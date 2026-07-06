module LibExceptions where
    
import Control.Exception (Exception) 
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void (Void)
import CommandExceptions (CommandException)

data ApplicationException = IOTableException { mess :: String } |
                            ParseException { bundle :: ParseErrorBundle String Void  } |
                            CmdException { exc :: CommandException } deriving (Show, Eq)
        

instance Exception ApplicationException