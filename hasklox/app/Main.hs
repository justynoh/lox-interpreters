module Main where

import qualified System.Environment as Env
import qualified Lox

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [] -> Lox.runPrompt ()
    [path] -> Lox.runFile path
    _ -> putStrLn "Usage: hasklox [script]"