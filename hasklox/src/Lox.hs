module Lox where

import qualified Types.Error as E
import qualified Scanner

run :: String -> IO Int
run program = 
  let tokens = Scanner.scanTokens program
  in do 
    putStrLn (concatMap (\t -> show t ++ "\n") tokens)
    return 0

-- TODO: Do something with this error function! 
error :: E.Error -> IO ()
error error = 
  case error of 
    E.ScanError msg lexeme line -> report "ScanError" line "" (msg ++ "(" ++ lexeme ++ ")")

report :: String -> Int -> String -> String -> IO ()
report errorType line at message =
  do
    putStrLn ("[line " ++ show line ++ "] Error(" ++ show errorType ++ ")" ++ at ++ ": " ++ message)
    Prelude.error "Hmm"