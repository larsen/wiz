module Wiz.Types (
    Formals( Formals ),
    Expression(
        Number,
        Boolean,
        Operator,
        Symbol,
        Quote,
        Cond,
        Clause,
        Definition,
        Lambda,
        List,
        SetInstruction,
        SetCarInstruction,
        SetCdrInstruction),
    Form (FExpr),
    Program (Program),
  ) where

import qualified Data.List as L
import qualified Data.Map as Map

data Formals = Formals [String]
  deriving (Eq, Show, Ord)

data Expression = Number Integer
                | Boolean Bool
                | Operator String
                | Symbol String
                | Quote Expression
                | Cond [Expression]
                | Clause Expression Expression
                | Definition String Expression
                | Lambda Formals Expression
                | List [Expression]
                | SetInstruction String Expression
                | SetCarInstruction String Expression
                | SetCdrInstruction String Expression
                  deriving (Eq, Ord)

instance Show Expression where
  show (Number n) = show n
  show (Boolean b) = if b then "#t" else "#f"
  show (Symbol s) = show s
  show (Quote e) = show e
  show (List exprs) = "(" ++ L.unwords (map show exprs) ++ ")"
  show (Operator c) = show c
  show (Definition sym expr) =
    "(define " ++ sym ++ " (" ++ show expr ++ "))"
  show (Lambda (Formals formals) expr) = "#PROCEDURE (λ (" ++ show expr ++ "))"

-- data Definition = Definition String Expression
--   deriving (Show)

data Form = FExpr Expression
  deriving (Eq, Show)

data Program = Program [Form]

