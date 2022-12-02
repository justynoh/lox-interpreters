module Interpreter where

import qualified Types.Ast as A  
import Utils.Error (Error(RuntimeError), throw)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.Map as M
import Control.Monad (join)

-- Env utils
type EnvScope = M.Map String (Maybe A.Lit)
type Env = (EnvScope, [EnvScope])

emptyEnv :: Env
emptyEnv = (M.empty, [])

declared :: String -> Env -> Bool
declared k (currentEnv, envStack) = 
  let find = M.member k in 
  foldl (\b env -> b || find env) (find currentEnv) envStack

declaredInScope :: String -> Env -> Bool
declaredInScope k (currentEnv, _) = M.member k currentEnv

declare :: String -> Env -> Env
declare k (currentEnv, envStack) = (M.insert k Nothing currentEnv, envStack)

declareAndAssign :: String -> A.Lit -> Env -> Env
declareAndAssign k v (currentEnv, envStack) = (M.insert k (Just v) currentEnv, envStack)

assign :: String -> A.Lit -> Env -> Env
assign k v (currentEnv, envStack) = 
  if M.member k currentEnv then (M.insert k (Just v) currentEnv, envStack) else
  let updateStack envStack = 
        case envStack of 
          [] -> error "Interpreter error: failed to update var."
          env:envs -> if M.member k env then M.insert k (Just v) env:envs else env:updateStack envs
  in 
  (currentEnv, updateStack envStack)

lookupValue :: Env -> String -> Maybe A.Lit
lookupValue (currentEnv, envStack) k = 
  let find env = join (M.lookup k env) in
  foldl (\v env -> case v of {Nothing -> find env; _ -> v}) (find currentEnv) envStack

push :: Env -> Env
push (env, envs) = (M.empty, env:envs)

pop :: Env -> Env
pop (_, envStack) =
  case envStack of 
    [] -> error "Interpreter error: failed to rollback env."
    env:envs -> (env, envs)
--

interpret :: Env -> A.Prog -> IO Env
interpret = interpretProg

interpretProg :: Env -> A.Prog -> IO Env
interpretProg env p = 
  case p of 
    [] -> return env
    s:ss -> do env' <- interpretBlkStmt env s; interpretProg env' ss

interpretBlkStmt :: Env -> A.BlkStmt -> IO Env
interpretBlkStmt env s =
  case s of 
    A.Decl id -> 
      if declaredInScope id env 
      then throw (RuntimeError ("Variable " ++ id ++ " declared twice within the same scope.")) 
      else return (declare id env)
    A.DeclAssn id exp -> 
      let (v, env') = evaluateExp env exp in 
      if declaredInScope id env 
      then throw (RuntimeError ("Variable " ++ id ++ " declared twice within the same scope.")) 
      else return (declareAndAssign id v env')
    A.Stmt stmt -> interpretStmt env stmt

interpretStmt :: Env -> A.Stmt -> IO Env
interpretStmt env s = 
  case s of
    A.PrintStmt e -> 
      let (v, env') = evaluateExp env e in do  
      putStrLn $ 
        case v of 
          A.Nil -> "nil"
          A.Boolean b -> if b then "true" else "false"
          A.Number n -> show n
          A.String s -> s
      return env'
    A.ExpStmt e -> do (_, env') <- evaluate (force (evaluateExp env e)); return env'
    A.Block stmts -> do env' <- foldl (\e s -> e >>= (`interpretBlkStmt` s)) (return (push env)) stmts; return (pop env')
    A.IfElseStmt e s1 s2 -> 
      let (v, env') = evaluateExp env e in 
      if isTruthy v then interpretStmt env' s1 else maybe (return env') (interpretStmt env') s2
    A.WhileStmt e s' ->
      let (v, env') = evaluateExp env e in
      if isTruthy v then do env'' <- interpretStmt env' s'; interpretStmt env'' s else return env

evaluateExp :: Env -> A.Exp -> (A.Lit, Env)
evaluateExp env e = 
  case e of 
    A.PureExp e' -> evaluateTernexp env e'
    A.AssnExp lval e' -> 
      let (v, env') = evaluateExp env e'
      in (v, 
          case lval of 
            A.IdentLvalue id -> 
              if declared id env'
              then assign id v env'
              else throw (RuntimeError ("Undefined variable '" ++ id ++ "'."))
          )

evaluateTernexp :: Env -> A.Ternexp -> (A.Lit, Env)
evaluateTernexp env e =
  case e of 
    A.TernexpNode e0 e1 e2 -> 
      let (v0 , env0) = evaluateOrexp env e0 in 
      evaluateTernexp env0 (if isTruthy v0 then e1 else e2)
    A.TernexpLeaf e' -> evaluateOrexp env e'

evaluateOrexp :: Env -> A.Orexp -> (A.Lit, Env)
evaluateOrexp env e =
  case e of 
    A.OrexpNode e1 e2 -> 
      let (v1, env1) = evaluateOrexp env e1 in 
      if isTruthy v1 then (v1, env1) else evaluateAndexp env1 e2
    A.OrexpLeaf e' -> evaluateAndexp env e'

evaluateAndexp :: Env -> A.Andexp -> (A.Lit, Env)
evaluateAndexp env e =
  case e of 
    A.AndexpNode e1 e2 -> 
      let (v1, env1) = evaluateAndexp env e1 in
      if not (isTruthy v1) then (v1, env1) else evaluateBinexp1 env1 e2
    A.AndexpLeaf e' -> evaluateBinexp1 env e'

evaluateBinexp1 :: Env -> A.Binexp1 -> (A.Lit, Env)
evaluateBinexp1 env e =
  case e of
    A.Binexp1Node e1 op e2 -> 
      let 
        (v1, env1) = evaluateBinexp1 env e1
        (v2, env2) = evaluateBinexp2 env1 e2
        eq = isEqual v1 v2
      in (case op of 
            A.Equal -> A.Boolean eq
            A.NotEqual -> A.Boolean (not eq)
          , env2)
    A.Binexp1Leaf e' -> evaluateBinexp2 env e'

evaluateBinexp2 :: Env -> A.Binexp2 -> (A.Lit, Env)
evaluateBinexp2 env e =
  case e of
    A.Binexp2Node e1 op e2 -> 
      let 
        opstr = case op of {A.Less -> "<"; A.LessEqual -> "<="; A.Greater -> ">"; A.GreaterEqual -> ">="}
        (v1, env1) = evaluateBinexp2 env e1
        (v2, env2) = evaluateBinexp3 env1 e2
        n1 = getNumber opstr v1
        n2 = getNumber opstr v2
      in (case op of 
            A.Less -> A.Boolean (n1 < n2)
            A.LessEqual -> A.Boolean (n1 <= n2)
            A.Greater -> A.Boolean (n1 > n2)
            A.GreaterEqual -> A.Boolean (n1 >= n2)
          , env2)
    A.Binexp2Leaf e' -> evaluateBinexp3 env e'


evaluateBinexp3 :: Env -> A.Binexp3 -> (A.Lit, Env)
evaluateBinexp3 env e =
  case e of
    A.Binexp3Node e1 op e2 -> 
      let 
        (v1, env1) = evaluateBinexp3 env e1
        (v2, env2) = evaluateBinexp4 env1 e2
      in (case op of 
            A.Plus -> 
              case (v1, v2) of
                (A.String s1, A.String s2) -> A.String (s1 ++ s2)
                (A.Number n1, A.Number n2) -> A.Number (n1 + n2)
                _ -> throw (RuntimeError "Operands of + must either both be numbers or both be strings.")
            A.Minus -> A.Number (getNumber "-" v1 - getNumber "-" v2)
          , env2)
    A.Binexp3Leaf e' -> evaluateBinexp4 env e'


evaluateBinexp4 :: Env -> A.Binexp4 -> (A.Lit, Env)
evaluateBinexp4 env e =
  case e of
    A.Binexp4Node e1 op e2 -> 
      let 
        opstr = case op of {A.Times -> "*"; A.Divide -> "/"}
        (v1, env1) = evaluateBinexp4 env e1
        (v2, env2) = evaluateUnexp env1 e2
        n1 = getNumber opstr v1
        n2 = getNumber opstr v2
      in (case op of 
            A.Times -> A.Number (n1 * n2)
            A.Divide -> if n2 == 0 then throw (RuntimeError "Division by zero.") 
                        else A.Number (n1 / n2)
          , env2)
    A.Binexp4Leaf e' -> evaluateUnexp env e'

evaluateUnexp :: Env -> A.Unexp -> (A.Lit, Env)
evaluateUnexp env e =
  case e of 
    A.UnexpNode op e' -> 
      let (v, env') = evaluateUnexp env e' 
      in (case op of
            A.Neg -> A.Number (- (getNumber "-" v))
            A.Not -> A.Boolean (not (isTruthy v))
          , env')
    A.UnexpLeaf e' -> evaluatePrim env e'

evaluatePrim :: Env -> A.Prim -> (A.Lit, Env)
evaluatePrim env p = 
  case p of 
    A.LitPrim l -> (l, env)
    A.ExpPrim e -> evaluateExp env e
    A.IdentPrim id -> 
      if declared id env 
      then (case lookupValue env id of 
              Nothing -> throw (RuntimeError ("Using uninitialized variable '" ++ id ++ "'.")) 
              Just v -> v
            , env) 
      else throw (RuntimeError ("Using undeclared variable '" ++ id ++ "'."))

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
