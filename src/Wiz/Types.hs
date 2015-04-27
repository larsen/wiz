module Wiz.Types (
    Formals( Formals ),
    Expression( Number, Boolean, Operator, Symbol, Quote, If, Lambda, List ),
    Definition( Definition ),
    Form (FDef, FExpr),
    Program (Program),
    Environment( Environment )
  ) where

import qualified Data.List as L
import qualified Data.Map as Map

data Formals = Formals [String]
  deriving (Eq, Show)

data Expression = Number Integer
                | Boolean Bool
                | Operator Char
                | Symbol String
                | Quote Expression
                | If Expression Expression Expression
                | Lambda Formals Expression
                | List [Expression]
                  deriving (Eq)

instance Show Expression where
  show (Number n) = show n
  show (Boolean b) = if b then "#t" else "#f"
  show (Symbol s) = show s
  show (Quote e) = show e
  show (List exprs) = "(" ++ L.unwords (map show exprs) ++ ")"
  show (Operator c) = show c
  show (If test consequent alternate) =
    "(if " ++ show test ++ " " ++ show consequent ++ " " ++ show alternate ++ ")"
  show (Lambda (Formals formals) expr) =
    "(λ (" ++ (L.unwords (map show formals)) ++ ") (" ++ show expr ++ "))"

data Definition = Definition String Expression
  deriving (Show)

data Form = FDef Definition | FExpr Expression
  deriving (Show)

data Environment = Environment (Map.Map String Expression)
                   deriving (Eq)

data Program = Program [Form]

instance Show Environment where
  show (Environment env) =
    L.unlines (map showPair (Map.toList env)) ++ "\n"
    where showPair (k, v) = show k ++ "\t->\t" ++ show v
