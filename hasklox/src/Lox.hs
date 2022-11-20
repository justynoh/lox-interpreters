module Lox where

import Utils.Error

import qualified Scanner
import qualified Parser
import qualified Interpreter

quitIf :: Bool -> Int -> IO Int -> IO Int
quitIf b n cont = if b then return n else cont

run :: String -> IO Int
run program = do
  -- Scanning
  (labelledTokens, errs) <- return (Scanner.scan program)
  -- Print errors, if any.
  foldl (\_ err -> putStrLn err) (return ()) errs
  if not (null errs) then return 1 else do
    maybeParsed <- catch (return (Just (Parser.parse labelledTokens))) Nothing
    case maybeParsed of 
      Nothing -> return 1
      Just parsed -> catch (Interpreter.interpret parsed) 1