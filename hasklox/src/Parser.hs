module Parser where

import qualified Types.Token as T
import qualified Types.Ast as A
import Control.Exception (throw, Exception)

data ParseException =
  ExceptionExp String
  deriving Show

instance Exception ParseException

parse :: T.LabelledTokens -> (A.Exp, T.LabelledTokens) -- Change this when done
parse = parseExp

parseExp :: T.LabelledTokens -> (A.Exp, T.LabelledTokens)
parseExp toks = 
  let (e, toks') = parseTernexp toks
  in (A.Exp e, toks')

parseTernexp :: T.LabelledTokens -> (A.Ternexp, T.LabelledTokens)
parseTernexp toks = 
  let (e0, toks') = parseBinexp1 toks in
  case toks' of
    (T.Question,_,_):ts -> parseTernexp' e0 ts
    _ -> (A.TernexpLeaf e0, toks')
  

parseTernexp' :: A.Binexp1 -> T.LabelledTokens -> (A.Ternexp, T.LabelledTokens)
parseTernexp' e0 toks = 
  let (e1, toks') = parseTernexp toks in
  case toks' of
    (T.Colon, _, _):ts -> let (e2, toks'') = parseTernexp ts in (A.TernexpNode e0 e1 e2, toks'')
    _ -> throw (ExceptionExp "Missing ':' after '?'.")

parseBinexp1 :: T.LabelledTokens -> (A.Binexp1, T.LabelledTokens)
parseBinexp1 = parseBinexp1' Nothing

parseBinexp1' :: Maybe (A.Binexp1, A.Binop1) -> T.LabelledTokens -> (A.Binexp1, T.LabelledTokens)
parseBinexp1' prev toks = 
  let (e1, toks') = parseBinexp2 toks in
  case toks' of
    [] -> maybe (A.Binexp1Leaf e1, []) (\(e0, op) -> (A.Binexp1Node e0 op e1, [])) prev 
    (t, _, _):ts ->
      let 
        currexp = maybe (A.Binexp1Leaf e1) (\(e0, op) -> A.Binexp1Node e0 op e1) prev
        loop op = parseBinexp1' (Just (currexp, op)) ts 
      in
        case t of 
          T.BangEqual -> loop A.NotEqual
          T.EqualEqual -> loop A.Equal
          _ -> (currexp, toks')

parseBinexp2 :: T.LabelledTokens -> (A.Binexp2, T.LabelledTokens)
parseBinexp2 = parseBinexp2' Nothing

parseBinexp2' :: Maybe (A.Binexp2, A.Binop2) -> T.LabelledTokens -> (A.Binexp2, T.LabelledTokens)
parseBinexp2' prev toks = 
  let (e1, toks') = parseBinexp3 toks in
  case toks' of
    [] -> maybe (A.Binexp2Leaf e1, []) (\(e0, op) -> (A.Binexp2Node e0 op e1, [])) prev 
    (t, _, _):ts ->
      let 
        currexp = maybe (A.Binexp2Leaf e1) (\(e0, op) -> A.Binexp2Node e0 op e1) prev
        loop op = parseBinexp2' (Just (currexp, op)) ts 
      in
        case t of 
          T.Less -> loop A.Less
          T.LessEqual -> loop A.LessEqual
          T.Greater -> loop A.Greater
          T.GreaterEqual -> loop A.GreaterEqual
          _ -> (currexp, toks')

parseBinexp3 :: T.LabelledTokens -> (A.Binexp3, T.LabelledTokens)
parseBinexp3 = parseBinexp3' Nothing

parseBinexp3' :: Maybe (A.Binexp3, A.Binop3) -> T.LabelledTokens -> (A.Binexp3, T.LabelledTokens)
parseBinexp3' prev toks = 
  let (e1, toks') = parseBinexp4 toks in
  case toks' of
    [] -> maybe (A.Binexp3Leaf e1, []) (\(e0, op) -> (A.Binexp3Node e0 op e1, [])) prev 
    (t, _, _):ts ->
      let 
        currexp = maybe (A.Binexp3Leaf e1) (\(e0, op) -> A.Binexp3Node e0 op e1) prev
        loop op = parseBinexp3' (Just (currexp, op)) ts 
      in
        case t of 
          T.Plus -> loop A.Plus
          T.Minus -> loop A.Minus
          _ -> (currexp, toks')

parseBinexp4 :: T.LabelledTokens -> (A.Binexp4, T.LabelledTokens)
parseBinexp4 = parseBinexp4' Nothing

parseBinexp4' :: Maybe (A.Binexp4, A.Binop4) -> T.LabelledTokens -> (A.Binexp4, T.LabelledTokens)
parseBinexp4' prev toks = 
  let (e1, toks') = parseUnexp toks in
  case toks' of
    [] -> maybe (A.Binexp4Leaf e1, []) (\(e0, op) -> (A.Binexp4Node e0 op e1, [])) prev 
    (t, _, _):ts ->
      let 
        currexp = maybe (A.Binexp4Leaf e1) (\(e0, op) -> A.Binexp4Node e0 op e1) prev
        loop op = parseBinexp4' (Just (currexp, op)) ts 
      in
        case t of 
          T.Star -> loop A.Times
          T.Slash -> loop A.Divide
          _ -> (currexp, toks')

parseUnexp :: T.LabelledTokens -> (A.Unexp, T.LabelledTokens)
parseUnexp toks =
  case toks of
    (T.Bang,_,_):ts -> let (e, ts') = parseUnexp ts in (A.UnexpNode A.Not e, ts')
    (T.Minus,_,_):ts -> let (e, ts') = parseUnexp ts in (A.UnexpNode A.Neg e, ts')
    _ -> let (e, ts) = parsePrim toks in (A.UnexpLeaf e, ts)

parsePrim :: T.LabelledTokens -> (A.Prim, T.LabelledTokens)
parsePrim toks = 
  case toks of 
    [] -> throw (ExceptionExp "EOF while parsing expression.")
    (t,_,_):ts ->
      case t of
        T.False -> (A.Literal (A.Boolean False), ts)
        T.True -> (A.Literal (A.Boolean True), ts)
        T.Nil -> (A.Literal A.Nil, ts)
        T.String s -> (A.Literal (A.String s), ts)
        T.Number n -> (A.Literal (A.Number n), ts)
        T.LeftParen -> 
          let 
            (e,toks') = parseExp ts
          in 
            case toks' of 
              (T.RightParen,_,_):ts' -> (A.Expression e, ts')
              _ -> throw (ExceptionExp "Missing ')' at end of parenthesized expression.")
        _ -> throw (ExceptionExp "Expected expression.")

synchronize :: T.LabelledTokens -> T.LabelledTokens
synchronize toks =
  case toks of 
    [] -> []
    (t,_,_):ts -> 
      case t of 
        T.Semicolon -> ts
        T.Class -> toks
        T.Fun -> toks
        T.Var -> toks
        T.For -> toks
        T.If -> toks
        T.While -> toks
        T.Print -> toks 
        T.Return -> toks
        _ -> synchronize ts
