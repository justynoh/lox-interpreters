module Main where

import qualified System.Environment as Env
import qualified System.IO as IO
import qualified Lox

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [] -> runPrompt ()
    [path] -> runFile path
    _ -> putStrLn "Usage: hasklox [script]"

runPrompt :: () -> IO ()
runPrompt () = do
  putStr "> "
  IO.hFlush IO.stdout
  line <- IO.getLine
  Lox.run line
  runPrompt ()

runFile :: String -> IO ()
runFile path = do
  contents <- readFile path
  error <- Lox.run contents
  return () -- hmmm...