module Types.Ast where 

data Unop = Neg | Not deriving Show
data Binop4 = Times | Divide deriving Show
data Binop3 = Plus | Minus deriving Show
data Binop2 = Less | LessEqual | Greater  | GreaterEqual deriving Show
data Binop1 = Equal | NotEqual deriving Show

data Lit = 
    Number Double
  | String String 
  | Boolean Bool
  | Nil
  deriving Show

data Prim = 
    Literal Lit
  | Expression Exp
  deriving Show

data Unexp =
    UnexpNode Unop Unexp
  | UnexpLeaf Prim
  deriving Show

data Binexp4 =
    Binexp4Node Binexp4 Binop4 Unexp
  | Binexp4Leaf Unexp
  deriving Show

data Binexp3 =
    Binexp3Node Binexp3 Binop3 Binexp4
  | Binexp3Leaf Binexp4
  deriving Show

data Binexp2 = 
    Binexp2Node Binexp2 Binop2 Binexp3
  | Binexp2Leaf Binexp3
  deriving Show

data Binexp1 =
    Binexp1Node Binexp1 Binop1 Binexp2
  | Binexp1Leaf Binexp2
  deriving Show

data Exp = 
  Binexp Binexp1
  deriving Show