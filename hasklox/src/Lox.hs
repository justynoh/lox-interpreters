module Lox where

import qualified Utils.Error as E
import qualified Scanner
import qualified Parser

quitIf :: Bool -> Int -> IO Int -> IO Int
quitIf b n cont = if b then return n else cont

run :: String -> IO Int
run program = do 
  -- Scanning
  (labelledTokens, errs) <- return (Scanner.scan program)
    -- Print errors, if any.
  foldl (\_ err -> putStrLn err) (return ()) errs
  (parsed, _) <- return (Parser.parse labelledTokens)
  print parsed
  return 0

  
  