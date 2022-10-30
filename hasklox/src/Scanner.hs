module Scanner where

import Data.Char (isDigit, isAlpha, isAlphaNum)
import Text.Read (readMaybe)
import qualified Types.Token as T

-- Scanner = (<i>, <line>, <program[i:]>)
type Scanner = (Int, Int, String)

-- ScannedToken_ = Token <token> | Error <msg>
data ScannedToken_ = Token T.Token | Error String deriving Show               
-- ScannedToken = (<token>,  <lexeme>, <line>)
type ScannedToken = (ScannedToken_, String, Int)

scanTokens :: String -> [ScannedToken]
scanTokens program = scanTokens' (0, 1, program)

scanTokens' :: Scanner -> [ScannedToken]
scanTokens' scanner = 
  let (t, scanner') = scanToken scanner
      ts = maybe [] scanTokens' scanner'
  in 
    maybe ts (:ts) t

scanToken :: Scanner -> (Maybe ScannedToken, Maybe Scanner)
scanToken scanner@(i, line, program) =
  -- Helper functions to make the big case less clunky
  let advanceZero :: String -> (Maybe ScannedToken, Maybe Scanner)
      advanceZero cs = (Nothing, Just (i + 1, line, cs))
      advanceOne :: T.Token -> Char -> String -> (Maybe ScannedToken, Maybe Scanner)
      advanceOne t c cs = (Just (Token t, [c], line), Just (i + 1, line, cs)) 
      advanceTwo :: T.Token -> Char -> Char -> String -> (Maybe ScannedToken, Maybe Scanner)
      advanceTwo t c1 c2 cs = (Just (Token t, [c1,c2], line), Just (i + 2, line, cs))
  in 
    case program of
      [] -> (Just (Token T.EOF, [], line), Nothing)
      c:cs -> 
        case c of 
          '(' -> advanceOne T.LeftParen c cs
          ')' -> advanceOne T.RightParen c cs
          '{' -> advanceOne T.LeftBrace c cs 
          '}' -> advanceOne T.RightBrace c cs 
          ',' -> advanceOne T.Comma c cs 
          '.' -> advanceOne T.Dot c cs 
          ';' -> advanceOne T.Semicolon c cs 
          '+' -> advanceOne T.Plus c cs 
          '-' -> advanceOne T.Minus c cs 
          '*' -> advanceOne T.Star c cs
          '/' -> 
            case cs of 
              -- If it's a comment, need to consume till EOL
              '/':cs' -> 
                let len = length (takeWhile (/='\n') program)
                in (Nothing, Just (i + len, line, drop len program))
              _ -> advanceOne T.Slash c cs
          '!' -> 
            case cs of 
              '=':cs' -> advanceTwo T.BangEqual c '=' cs'
              _ -> advanceOne T.Bang c cs
          '=' -> 
            case cs of 
              '=':cs' -> advanceTwo T.EqualEqual c '=' cs'
              _ -> advanceOne T.Equal c cs
          '>' ->
            case cs of 
              '=':cs' -> advanceTwo T.GreaterEqual c '=' cs'
              _ -> advanceOne T.Greater c cs
          '<' -> 
            case cs of 
              '=':cs' -> advanceTwo T.LessEqual c '=' cs'
              _ -> advanceOne T.Less c cs
                  -- Can't use advanceZero because we need to increment line
          '\n' -> (Nothing, Just (i + 1, line + 1, cs)) 
          ' ' -> advanceZero cs
          '\r' -> advanceZero cs
          '\t' -> advanceZero cs
          '"' -> scanString [] (i + 1, line, cs)
          _ -> 
            if isDigit c then scanNumber False [] scanner else 
            if isAlpha c || c == '_' then scanIdentOrKeyword [] scanner else
            (Just (Error "Unknown token.", [c], line), Just (i + 1, line, cs))

scanString :: String -> Scanner -> (Maybe ScannedToken, Maybe Scanner)
scanString s scanner@(i, line, program) =
  case program of 
    [] -> (Just (Error "EOF while scanning string literal.", reverse s, line), Just scanner)
    '"':cs -> (Just (Token (T.String (reverse s)), '"':reverse ('"':s), line), Just (i+1, line, cs))
    c:cs -> scanString (c:s) (i+1, line + (if c == '\n' then 1 else 0), cs)

scanNumber :: Bool -> String -> Scanner -> (Maybe ScannedToken, Maybe Scanner)
scanNumber isFrac s scanner@(i, line, program) = 
  let parseNumber :: () -> (Maybe ScannedToken, Maybe Scanner)
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
          scanNumber True (c:s) (i + 1, line, cs) 
        else
          if isDigit c then
            -- Not in fraction mode, and we don't see a '.'.
            scanNumber isFrac (c:s) (i + 1, line, cs)
          else
            -- Once we stop reading digits and '.'s, do the parse.
            parseNumber ()

scanIdentOrKeyword :: String -> Scanner -> (Maybe ScannedToken, Maybe Scanner)
scanIdentOrKeyword s scanner@(i, line, program) =
  let parseIdentOrKeyword () = 
        let s' = reverse s
        in (Just (Token (keywordMap s'), s', line), Just scanner) 
  in 
    case program of 
      [] -> if null s then error "Compiler error." else parseIdentOrKeyword ()
      c:cs ->
        if isAlphaNum c || c == '_' then 
          scanIdentOrKeyword (c:s) (i+1, line, cs) 
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
    
