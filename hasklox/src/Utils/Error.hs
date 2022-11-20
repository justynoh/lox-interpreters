module Utils.Error where

import qualified Control.Exception as E

data Error = 
    ScanError String String Int
  | ParseError String Int
  | RuntimeError String
  deriving Show

instance E.Exception Error

toString :: Error -> String
toString e = 
  case e of 
    ScanError msg lexeme line -> report "scan" line (msg ++ " (lexeme \"" ++ lexeme ++ "\")")
    ParseError msg line -> report "parse" line msg
    RuntimeError msg -> "Runtime error: " ++ msg

catch :: IO a -> a -> IO a
catch s k = E.catch s (\e -> do putStrLn (toString e); return k)

throw :: Error -> a
throw = E.throw

report :: String -> Int -> String -> String
report at line message = 
  (if line == 0 then "" else "[line " ++ show line ++ "] ") 
  ++ "Error at <" 
  ++ at 
  ++ ">: " 
  ++ message