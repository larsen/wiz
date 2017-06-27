module Wiz.Types (
    Formals( Formals ),
    Expression(
        Number,
        Boolean,
        Operator,
        Symbol,
        String,
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

data Expression = Number Double
                | Boolean Bool
                | Operator String
                | Symbol String
                | String String
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
  show (String s) = show s
  show (Boolean b) = if b then "#t" else "#f"
  show (Symbol s) = show s
  show (Quote e) = "'" ++ show e
  show (Cond exprs) = "(cond "++ L.unwords (map show exprs) ++ ")"
  show (Clause t c) = "(" ++ show t ++ " " ++ show c ++ ")"
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

