module Wiz.Types (
    Formals( Formals ),
    Expression( Number, Operator, Symbol, If, Lambda, LambdaApply, List ),
    Definition( Definition ),
    Form (FDef, FExpr),
    Environment( Environment )
  ) where

import qualified Data.List as L
import qualified Data.Map as Map

data Formals = Formals [String]
  deriving (Show)

data Expression = Number Integer
                | Operator Char [Expression]
                | Symbol String
                | If Expression Expression Expression
                | Lambda Formals Expression
                | LambdaApply String [Expression]
                | List [Expression]

-- TODO ora mi piacerebbe implementare
-- car e cdr

instance Show Expression where
  show (Number n) = show n
  show (Symbol s) = show s
  show (List exprs) = "( " ++ L.unwords (map show exprs) ++ " )"
  show (Operator c exprs) =
    "( " ++ show c ++ " " ++ L.unwords (map show exprs) ++ " )"
  show (If test consequent alternate) =
    "(if " ++ show test ++ " " ++ show consequent ++ " " ++ show alternate ++ ")"
  show (Lambda (Formals formals) expr) =
    "(λ ( " ++ (L.unwords (map show formals)) ++ " ) ( " ++ show expr ++ " ) )"
  show (LambdaApply s exprs) =
    "( " ++ show s ++ " " ++ (L.unwords (map show exprs)) ++ " )"

data Definition = Definition String Expression
  deriving (Show)

data Form = FDef Definition | FExpr Expression
  deriving (Show)

data Environment = Environment (Map.Map String Expression)

instance Show Environment where
  show (Environment env) =
    L.unlines (map showPair (Map.toList env)) ++ "\n"
    where showPair (k, v) = show k ++ "\t->\t" ++ show v
