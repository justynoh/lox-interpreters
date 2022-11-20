module Scanner where

import Data.Char (isDigit, isAlpha, isAlphaNum)
import Text.Read (readMaybe)
import qualified Utils.Error as E
import qualified Types.Token as T

-- Scanner = (<line>, <program[i:]>)
type Scanner = (Int, String)

-- TryToken = Token <token> | Error <msg>
data TryToken = Token T.Token | Error String
-- LabelledTryToken = (<trytoken>,  <lexeme>, <line>)
type LabelledTryToken = (TryToken, String, Int)
type LabelledTryTokens = [LabelledTryToken]

scan :: String -> (T.LabelledTokens, [String])
scan program = filterTokens (scanTokens (1, program))

scanTokens :: Scanner -> LabelledTryTokens
scanTokens scanner = 
  let (t, scanner') = scanToken scanner
      ts = maybe [] scanTokens scanner'
  in maybe ts (:ts) t

scanToken :: Scanner -> (Maybe LabelledTryToken, Maybe Scanner)
scanToken scanner@(line, program) =
  -- Helper functions to make the big case less clunky
  let 
    advance0 :: String -> (Maybe LabelledTryToken, Maybe Scanner)
    advance0 cs = (Nothing, Just (line, cs))
    advance :: T.Token -> String -> String -> (Maybe LabelledTryToken, Maybe Scanner)
    advance t lx cs = (Just (Token t, lx, line), Just (line, cs)) 
  in 
    case program of
      [] -> (Nothing, Nothing)
      c:cs -> 
        case c of 
          '(' -> advance T.LeftParen [c] cs
          ')' -> advance T.RightParen [c] cs
          '{' -> advance T.LeftBrace [c] cs 
          '}' -> advance T.RightBrace [c] cs 
          ',' -> advance T.Comma [c] cs 
          '.' -> advance T.Dot [c] cs 
          ';' -> advance T.Semicolon [c] cs 
          '+' -> advance T.Plus [c] cs 
          '-' -> advance T.Minus [c] cs 
          '*' -> advance T.Star [c] cs
          '?' -> advance T.Question [c] cs
          ':' -> advance T.Colon [c] cs
          '/' -> 
            case cs of 
              -- If it's a comment, need to consume till EOL
              '/':cs' -> 
                let len = length (takeWhile (/='\n') program)
                in (Nothing, Just (line, drop len program))
              _ -> advance T.Slash [c] cs
          '!' -> 
            case cs of 
              '=':cs' -> advance T.BangEqual [c,'='] cs'
              _ -> advance T.Bang [c] cs
          '=' -> 
            case cs of 
              '=':cs' -> advance T.EqualEqual [c,'='] cs'
              _ -> advance T.Equal [c] cs
          '>' ->
            case cs of 
              '=':cs' -> advance T.GreaterEqual [c,'='] cs'
              _ -> advance T.Greater [c] cs
          '<' -> 
            case cs of 
              '=':cs' -> advance T.LessEqual [c,'='] cs'
              _ -> advance T.Less [c] cs
          '\n' -> (Nothing, Just (line + 1, cs)) -- Can't use advanceZero because we need to increment line
          ' ' -> advance0 cs
          '\r' -> advance0 cs
          '\t' -> advance0 cs
          '"' -> scanString c [] (line, cs)
          '\'' -> scanString c [] (line, cs)
          _ -> 
            if isDigit c then scanNumber False [] scanner else 
            if isAlpha c || c == '_' then scanIdentOrKeyword [] scanner else
            (Just (Error "Unknown token.", [c], line), Just (line, cs))

scanString :: Char -> String -> Scanner -> (Maybe LabelledTryToken, Maybe Scanner)
scanString delimiter s scanner@(line, program) =
  case program of 
    [] -> (Just (Error "EOF while scanning string literal.", reverse s, line), Just scanner)
    c:cs -> 
      if c == delimiter 
      then (Just (Token (T.String (reverse s)), delimiter:reverse (delimiter:s), line), Just (line, cs)) 
      else scanString delimiter (c:s) (line + (if c == '\n' then 1 else 0), cs)

scanNumber :: Bool -> String -> Scanner -> (Maybe LabelledTryToken, Maybe Scanner)
scanNumber isFrac s scanner@(line, program) = 
  let 
    parseNumber :: () -> (Maybe LabelledTryToken, Maybe Scanner)
    parseNumber () = 
      let s' = reverse s
          d = readMaybe s' :: Maybe Double
      in (Just (maybe (Error "Error while parsing number.") (Token . T.Number) d, s', line), Just scanner)
  in
    case program of 
      [] -> 
        if null s || not (isDigit (head s)) then error "Compiler error." else parseNumber ()
      c:cs -> 
        if not isFrac && c == '.' && not (null cs) && isDigit (head cs) then
          -- If not already in fraction mode and we have '.x' where x is a digit, then switch into fraction mode.
          scanNumber True (c:s) (line, cs) 
        else
          if isDigit c then
            -- Not in fraction mode, and we don't see a '.'.
            scanNumber isFrac (c:s) (line, cs)
          else
            -- Once we stop reading digits and '.'s, do the parse.
            parseNumber ()

scanIdentOrKeyword :: String -> Scanner -> (Maybe LabelledTryToken, Maybe Scanner)
scanIdentOrKeyword s scanner@(line, program) =
  let 
    parseIdentOrKeyword :: () -> (Maybe LabelledTryToken, Maybe Scanner)
    parseIdentOrKeyword () = 
      let s' = reverse s
      in (Just (Token (keywordMap s'), s', line), Just scanner) 
  in 
    case program of 
      [] -> if null s then error "Compiler error." else parseIdentOrKeyword ()
      c:cs ->
        if isAlphaNum c || c == '_' then 
          scanIdentOrKeyword (c:s) (line, cs) 
        else parseIdentOrKeyword ()

keywordMap :: String -> T.Token
keywordMap s =
  case s of 
    "and" -> T.And
    "class" -> T.Class
    "else" -> T.Else
    "false" -> T.False 
    "for" -> T.For
    "fun" -> T.Fun
    "if" -> T.If
    "nil" -> T.Nil
    "or" -> T.Or
    "print" -> T.Print
    "return" -> T.Return
    "super" -> T.Super
    "this" -> T.This
    "true" -> T.True
    "var" -> T.Var
    "while" -> T.While
    _ -> T.Identifier s

filterTokens :: LabelledTryTokens -> (T.LabelledTokens, [String])
filterTokens [] = ([], [])
filterTokens ((trytoken, lexeme, line):ts) =
  let (toks, errs) = filterTokens ts
  in
    case trytoken of 
      Token token -> ((token, lexeme, line):toks, errs)
      Error msg -> (toks, E.toString (E.ScanError msg lexeme line):errs)

