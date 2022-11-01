module Utils.Error where

data Error = 
  ScanError String String Int
  deriving Show

error :: Error -> String
error e = 
  case e of 
    ScanError msg lexeme line -> report "ScanError" line "" (msg ++ "(" ++ lexeme ++ ")")

report :: String -> Int -> String -> String -> String
report errorType line at message =
  "[line " ++ show line ++ "] Error(" ++ show errorType ++ ")" ++ at ++ ": " ++ message