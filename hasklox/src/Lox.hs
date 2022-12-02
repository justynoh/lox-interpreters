module Lox where

import qualified System.IO as IO

import Utils.Error (catch)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Scanner
import qualified Parser
import qualified Interpreter

runPrompt :: () -> IO ()
runPrompt () = 
  let runPromptLoop :: Interpreter.Env -> IO ()
      runPromptLoop env = do
        putStr "> "
        IO.hFlush IO.stdout
        line <- IO.getLine
        env' <- run env line
        runPromptLoop env'
  in runPromptLoop Interpreter.emptyEnv

runFile :: String -> IO ()
runFile path = do
  contents <- readFile path
  error <- run Interpreter.emptyEnv contents
  return () -- hmmm...

quitIf :: Bool -> Int -> IO Int -> IO Int
quitIf b n cont = if b then return n else cont

run :: Interpreter.Env -> String -> IO Interpreter.Env
run env program = do
  -- Scanning
  (labelledTokens, errs) <- return (Scanner.scan program)
  -- Print errors, if any.
  foldl (\_ err -> putStrLn err) (return ()) errs
  if not (null errs) then return env else do
    -- Parsing
    maybeParsed <- catch (evaluate (force (Just (Parser.parse labelledTokens)))) Nothing
    case maybeParsed of 
      Nothing -> return env
      -- Interpret
      Just parsed -> catch (Interpreter.interpret env parsed) env