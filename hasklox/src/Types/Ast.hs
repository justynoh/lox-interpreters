{-# LANGUAGE DeriveGeneric #-}

module Types.Ast where 

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Unop = Neg | Not deriving (Show, Generic)
data Binop4 = Times | Divide deriving (Show, Generic)
data Binop3 = Plus | Minus deriving (Show, Generic)
data Binop2 = Less | LessEqual | Greater  | GreaterEqual deriving (Show, Generic)
data Binop1 = Equal | NotEqual deriving (Show, Generic)

data Lvalue = IdentLvalue String deriving (Show, Generic)

data Lit = 
    Number Double
  | String String 
  | Boolean Bool
  | Nil
  deriving (Show, Generic)

data Prim = 
    LitPrim Lit
  | IdentPrim String
  | ExpPrim Exp
  deriving (Show, Generic)

data Unexp =
    UnexpNode Unop Unexp
  | UnexpLeaf Prim
  deriving (Show, Generic)

data Binexp4 =
    Binexp4Node Binexp4 Binop4 Unexp
  | Binexp4Leaf Unexp
  deriving (Show, Generic)

data Binexp3 =
    Binexp3Node Binexp3 Binop3 Binexp4
  | Binexp3Leaf Binexp4
  deriving (Show, Generic)

data Binexp2 = 
    Binexp2Node Binexp2 Binop2 Binexp3
  | Binexp2Leaf Binexp3
  deriving (Show, Generic)

data Binexp1 =
    Binexp1Node Binexp1 Binop1 Binexp2
  | Binexp1Leaf Binexp2
  deriving (Show, Generic)

data Andexp =
    AndexpNode Andexp Binexp1
  | AndexpLeaf Binexp1
  deriving (Show, Generic)

data Orexp = 
    OrexpNode Orexp Andexp
  | OrexpLeaf Andexp
  deriving (Show, Generic)

data Ternexp =
    TernexpNode Orexp Ternexp Ternexp
  | TernexpLeaf Orexp
  deriving (Show, Generic)

data Exp = 
    AssnExp Lvalue Exp 
  | PureExp Ternexp 
  deriving (Show, Generic)

data Stmt = 
    PrintStmt Exp 
  | Block [BlkStmt]
  | ExpStmt Exp 
  | IfElseStmt Exp Stmt (Maybe Stmt)
  | WhileStmt Exp Stmt
  deriving (Show, Generic)

data BlkStmt = 
    Decl String 
  | DeclAssn String Exp 
  | Stmt Stmt 
  deriving (Show, Generic)

type Prog = [BlkStmt] 

-- Needed in order to force evaluation.
instance NFData Unop
instance NFData Binop4
instance NFData Binop3
instance NFData Binop2
instance NFData Binop1

instance NFData Lvalue

instance NFData Lit
instance NFData Prim
instance NFData Unexp
instance NFData Binexp4
instance NFData Binexp3
instance NFData Binexp2
instance NFData Binexp1
instance NFData Andexp
instance NFData Orexp
instance NFData Ternexp
instance NFData Exp
instance NFData Stmt
instance NFData BlkStmt
