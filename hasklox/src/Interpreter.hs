module Interpreter where

import Types.Ast as A  
import Control.Exception (throw, Exception)

data InterpretException =
  RuntimeError String
  deriving Show

instance Exception InterpretException

evaluate :: A.Exp -> A.Lit
evaluate = evaluateExp

evaluateExp :: A.Exp -> A.Lit
evaluateExp (A.Exp e) = evaluateTernexp e

evaluateTernexp :: A.Ternexp -> A.Lit
evaluateTernexp e =
  case e of 
    A.TernexpNode e0 e1 e2 -> if isTruthy (evaluateBinexp1 e0) then evaluateTernexp e1 else evaluateTernexp e2
    A.TernexpLeaf e' -> evaluateBinexp1 e'

evaluateBinexp1 :: A.Binexp1 -> A.Lit
evaluateBinexp1 e =
  case e of
    A.Binexp1Node e1 op e2 -> 
      let 
        lit1 = evaluateBinexp1 e1
        lit2 = evaluateBinexp2 e2
        eq = isEqual lit1 lit2
      in 
        case op of 
          A.Equal -> A.Boolean eq
          A.NotEqual -> A.Boolean (not eq)
    A.Binexp1Leaf e' -> evaluateBinexp2 e'

evaluateBinexp2 :: A.Binexp2 -> A.Lit
evaluateBinexp2 e =
  case e of
    A.Binexp2Node e1 op e2 -> 
      let 
        opstr = case op of {A.Less -> "<"; A.LessEqual -> "<="; A.Greater -> ">"; A.GreaterEqual -> ">="}
        n1 = getNumber opstr (evaluateBinexp2 e1)
        n2 = getNumber opstr (evaluateBinexp3 e2)
      in 
        case op of 
          A.Less -> A.Boolean (n1 < n2)
          A.LessEqual -> A.Boolean (n1 <= n2)
          A.Greater -> A.Boolean (n1 > n2)
          A.GreaterEqual -> A.Boolean (n1 >= n2)
    A.Binexp2Leaf e' -> evaluateBinexp3 e'


evaluateBinexp3 :: A.Binexp3 -> A.Lit
evaluateBinexp3 e =
  case e of
    A.Binexp3Node e1 op e2 -> 
      let 
        lit1 = evaluateBinexp3 e1
        lit2 = evaluateBinexp4 e2
      in 
        case op of 
          A.Plus -> 
            case (lit1, lit2) of
              (A.String s1, A.String s2) -> A.String (s1 ++ s2)
              (A.Number n1, A.Number n2) -> A.Number (n1 + n2)
              _ -> throw (RuntimeError "Operands of + must either both be numbers or both be strings.")
          A.Minus -> A.Number (getNumber "-" lit1 - getNumber "-" lit2)
    A.Binexp3Leaf e' -> evaluateBinexp4 e'


evaluateBinexp4 :: A.Binexp4 -> A.Lit
evaluateBinexp4 e =
  case e of
    A.Binexp4Node e1 op e2 -> 
      let 
        opstr = case op of {A.Times -> "*"; A.Divide -> "/"}
        n1 = getNumber opstr (evaluateBinexp4 e1)
        n2 = getNumber opstr (evaluateUnexp e2)
      in 
        case op of 
          A.Times -> A.Number (n1 * n2)
          A.Divide -> A.Number (n1 / n2)
    A.Binexp4Leaf e' -> evaluateUnexp e'

evaluateUnexp :: A.Unexp -> A.Lit
evaluateUnexp e =
  case e of 
    A.UnexpNode op e' -> 
      let lit = evaluateUnexp e' in 
      case op of
        A.Neg -> A.Number (- (getNumber "-" lit))
        A.Not -> A.Boolean (not (isTruthy lit))
    A.UnexpLeaf e' -> evaluatePrim e'

evaluatePrim :: A.Prim -> A.Lit
evaluatePrim (A.Literal l) = l
evaluatePrim (A.Expression e) = evaluateExp e

getNumber :: String -> A.Lit -> Double
getNumber op lit =
  case lit of 
    A.Number n -> n 
    _ -> throw (RuntimeError ("Operands of " ++ op ++ " must be numbers."))

isTruthy :: A.Lit -> Bool
isTruthy lit =
  -- Only false, nil, 0 and '' are falsey, everything is truthy.
  case lit of 
    A.Boolean b -> b
    A.Nil -> False 
    A.Number n -> n /= 0
    A.String s -> s /= "" 

isEqual :: A.Lit -> A.Lit -> Bool
isEqual lit1 lit2 =
  case (lit1, lit2) of
    (A.Nil, A.Nil) -> True
    (A.Boolean b1, A.Boolean b2) -> b1 == b2
    (A.String s1, A.String s2) -> s1 == s2
    (A.Number n1, A.Number n2) -> n1 == n2
    _ -> False
