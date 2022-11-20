module Types.Token where

data Token =
  -- Syntactic
    LeftParen
  | RightParen
  | LeftBrace
  | RightBrace 
  | Comma 
  | Equal 
  | Dot 
  | Semicolon
  -- Native functions
  | Plus 
  | Minus
  | Star
  | Slash
  | Bang 
  | EqualEqual
  | BangEqual
  | Greater 
  | GreaterEqual
  | Less
  | LessEqual
  | Question
  | Colon
  -- Literals
  | Identifier String
  | String String
  | Number Double
  -- Keywords
  | And 
  | Class 
  | Else
  | False 
  | Fun
  | For
  | If 
  | Nil 
  | Or 
  | Print 
  | Return 
  | Super 
  | This 
  | True 
  | Var
  | While 

-- LabelledToken = (<token>, <lexeme>, <line>)
type LabelledToken = (Token, String, Int)

type LabelledTokens = [LabelledToken]