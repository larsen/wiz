module Wiz.Types (
    Formals( Formals ),
    Expression(
        Number,
        Boolean,
        Operator,
        Symbol,
        Quote,
        If,
        Definition,
        Lambda,
        List ),
    Form (FExpr),
    Program (Program),
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
                | Definition String Expression
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
  show (Definition sym expr) =
    "(define " ++ sym ++ " (" ++ show expr ++ "))"
  show (Lambda (Formals formals) expr) = "#PROCEDURE (λ (" ++ show expr ++ "))"

-- data Definition = Definition String Expression
--   deriving (Show)

data Form = FExpr Expression
  deriving (Eq, Show)

data Program = Program [Form]

