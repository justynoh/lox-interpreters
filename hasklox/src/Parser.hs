module Parser where

import qualified Types.Token as T
import qualified Types.Ast as A
import Utils.Error (Error(ParseError), throw)

parse :: T.LabelledTokens -> A.Prog
parse = parseProg

parseProg :: T.LabelledTokens -> A.Prog
parseProg toks = 
  case toks of 
    [] -> []
    _ -> let (s, toks') = parseBlkStmt toks in s : parseProg toks'

parseBlkStmt :: T.LabelledTokens -> (A.BlkStmt, T.LabelledTokens)
parseBlkStmt toks =
  case toks of 
    (T.Var, _, line):ts -> 
      case ts of
        (T.Identifier ident, _, line'):ts' ->
          case ts' of
            (T.Semicolon, _, _):ts'' -> (A.Decl ident, ts'') -- Declare only
            (T.Equal, _, line''):ts'' -> -- Declare and assign
              let (exp, toks') = parseExp ts'' in
              case toks' of 
                (T.Semicolon, _, _):ts''' -> (A.DeclAssn ident exp, ts''')
                _ -> throw (ParseError "Expected ';' after variable assignment." line'')
            _ -> throw (ParseError "Expected ';' or '=' after variable declaration." line')
        _ -> throw (ParseError "Expected identified after 'var'." line)
    _ -> let (stmt, toks') = parseStmt toks in (A.Stmt stmt, toks')


parseStmt :: T.LabelledTokens -> (A.Stmt, T.LabelledTokens)
parseStmt toks =
  case toks of
    (T.Print, _, line):ts -> 
      let (e, toks') = parseExp ts in
      case toks' of
        (T.Semicolon, _, _):ts' -> (A.PrintStmt e, ts')
        _ -> throw (ParseError "Expected ';' after print expression." line)
    _ ->
      let (e, toks') = parseExp toks in
      case toks' of
        (T.Semicolon, _, _):ts' -> (A.ExpStmt e, ts')
        _ -> throw (ParseError "Expected ';' after expression statement." 0) -- TODO: Update line number later


parseExp :: T.LabelledTokens -> (A.Exp, T.LabelledTokens)
parseExp toks = 
  let (e, toks') = parseTernexp toks in 
  case toks' of 
    (T.Equal, _, line):ts' -> let (e1, toks'') = parseExp ts' in (A.AssnExp (ternexpToLvalue e line) e1, toks'')
    _ -> (A.PureExp e, toks')

parseTernexp :: T.LabelledTokens -> (A.Ternexp, T.LabelledTokens)
parseTernexp toks = 
  let (e0, toks') = parseBinexp1 Nothing toks in
  case toks' of
    (T.Question, _, line):ts ->
      let (e1, toks'') = parseTernexp ts in
      case toks'' of
        (T.Colon, _, _):ts' -> 
          let (e2, toks''') = parseTernexp ts' in 
          (A.TernexpNode e0 e1 e2, toks''')
        _ -> throw (ParseError "Expected ':' after '?'." line)
    _ -> (A.TernexpLeaf e0, toks')
  
parseBinexp1 :: Maybe (A.Binexp1, A.Binop1) -> T.LabelledTokens -> (A.Binexp1, T.LabelledTokens)
parseBinexp1 prev toks = 
  let (e1, toks') = parseBinexp2 Nothing toks in
  case toks' of
    [] -> maybe (A.Binexp1Leaf e1, []) (\(e0, op) -> (A.Binexp1Node e0 op e1, [])) prev 
    (t, _, _):ts ->
      let 
        currexp = maybe (A.Binexp1Leaf e1) (\(e0, op) -> A.Binexp1Node e0 op e1) prev
        loop op = parseBinexp1 (Just (currexp, op)) ts 
      in
        case t of 
          T.BangEqual -> loop A.NotEqual
          T.EqualEqual -> loop A.Equal
          _ -> (currexp, toks')

parseBinexp2 :: Maybe (A.Binexp2, A.Binop2) -> T.LabelledTokens -> (A.Binexp2, T.LabelledTokens)
parseBinexp2 prev toks = 
  let (e1, toks') = parseBinexp3 Nothing toks in
  case toks' of
    [] -> maybe (A.Binexp2Leaf e1, []) (\(e0, op) -> (A.Binexp2Node e0 op e1, [])) prev 
    (t, _, _):ts ->
      let 
        currexp = maybe (A.Binexp2Leaf e1) (\(e0, op) -> A.Binexp2Node e0 op e1) prev
        loop op = parseBinexp2 (Just (currexp, op)) ts 
      in
        case t of 
          T.Less -> loop A.Less
          T.LessEqual -> loop A.LessEqual
          T.Greater -> loop A.Greater
          T.GreaterEqual -> loop A.GreaterEqual
          _ -> (currexp, toks')

parseBinexp3 :: Maybe (A.Binexp3, A.Binop3) -> T.LabelledTokens -> (A.Binexp3, T.LabelledTokens)
parseBinexp3 prev toks = 
  let (e1, toks') = parseBinexp4 Nothing toks in
  case toks' of
    [] -> maybe (A.Binexp3Leaf e1, []) (\(e0, op) -> (A.Binexp3Node e0 op e1, [])) prev 
    (t, _, _):ts ->
      let 
        currexp = maybe (A.Binexp3Leaf e1) (\(e0, op) -> A.Binexp3Node e0 op e1) prev
        loop op = parseBinexp3 (Just (currexp, op)) ts 
      in
        case t of 
          T.Plus -> loop A.Plus
          T.Minus -> loop A.Minus
          _ -> (currexp, toks')

parseBinexp4 :: Maybe (A.Binexp4, A.Binop4) -> T.LabelledTokens -> (A.Binexp4, T.LabelledTokens)
parseBinexp4 prev toks = 
  let (e1, toks') = parseUnexp toks in
  case toks' of
    [] -> maybe (A.Binexp4Leaf e1, []) (\(e0, op) -> (A.Binexp4Node e0 op e1, [])) prev 
    (t, _, _):ts ->
      let 
        currexp = maybe (A.Binexp4Leaf e1) (\(e0, op) -> A.Binexp4Node e0 op e1) prev
        loop op = parseBinexp4 (Just (currexp, op)) ts 
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
    [] -> throw (ParseError "EOF while parsing expression." 0)
    (t, _, line):ts ->
      case t of
        T.False -> (A.LitPrim (A.Boolean False), ts)
        T.True -> (A.LitPrim (A.Boolean True), ts)
        T.Nil -> (A.LitPrim A.Nil, ts)
        T.String s -> (A.LitPrim (A.String s), ts)
        T.Number n -> (A.LitPrim (A.Number n), ts)
        T.Identifier id -> (A.IdentPrim id, ts)
        T.LeftParen -> 
          let 
            (e,toks') = parseExp ts
          in 
            case toks' of 
              (T.RightParen,_,_):ts' -> (A.ExpPrim e, ts')
              _ -> throw (ParseError "Expected ')' at end of parenthesized expression." line)
        _ -> throw (ParseError "Expected expression." line)

-- Resync parsing so that the parser can go back to a position it thinks it knows what's going on.
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

ternexpToLvalue :: A.Ternexp -> Int -> A.Lvalue
ternexpToLvalue e line = 
  case e of
    A.TernexpLeaf (A.Binexp1Leaf (A.Binexp2Leaf (A.Binexp3Leaf (A.Binexp4Leaf (A.UnexpLeaf (A.IdentPrim s)))))) -> A.IdentLvalue s
    _ -> throw (ParseError "Invalid assignment target on left side of '='." line)