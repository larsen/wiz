module Wiz.EvalApply (
    eval
  ) where

import Wiz.Types
import Wiz.Environment
import qualified Data.Map as Map
import Data.Maybe
import Text.Printf
import Debug.Trace

eval :: Form -> Environment -> (Environment, Maybe Expression)
eval (FExpr (Definition sym expr)) env = (evalDefinition (Definition sym expr) env, Nothing)
eval (FExpr expr) env = (env, Just $ evalExpr env expr)

evalDefinition :: Expression -> Environment -> Environment
evalDefinition (Definition symbol expr) env = extendEnvironment env [(symbol, expr)]

evalNum :: Expression -> Integer
evalNum (Number n) = n
evalNum e = error $ "evalNum " ++ show e

evalBool :: Expression -> Bool
evalBool (Boolean b) = b
evalBool e = error $ "evalBool " ++ show e

equal :: Expression -> Expression -> Expression
equal x y | x == y = Boolean True
          | otherwise = Boolean False

car :: Expression -> Expression
car (List (x:_)) = x
car (List []) = List []
car _ = error "car applied to non list expression!"

cdr :: Expression -> Expression
cdr (List (_:xs)) = List xs
cdr (List []) = List []
cdr _ = error "car applied to non list expression!"

cons :: Environment -> Expression -> Expression -> Expression
cons env x (List []) = List [evalExpr env x]
cons env x y = List [evalExpr env x, evalExpr env y]

nil :: Expression -> Expression
nil (List []) = Boolean True
nil _ = Boolean False

not :: Expression -> Expression
not (Boolean False) = Boolean True
not _ = Boolean False

or :: [Expression] -> Expression
or = Boolean . Prelude.or . map evalBool

pair :: Expression -> Expression
pair (List (x:_)) = Boolean True
pair _ = Boolean False

evalExpr :: Environment -> Expression -> Expression
-- evalExpr env expr
--   | trace ("evalExpr " ++ show expr ++ " in\n" ++ show env) False = undefined
evalExpr env expression =
  case expression of
    Number n -> Number n
    Boolean b -> Boolean b
    Symbol s -> evalExpr env (envLookup s env)
    
    List [] -> expression
    List exprs@(x:xs) ->
      case x of
        Operator '*' -> Number (product (map (evalNum . evalExpr env) xs))
        Operator '+' -> Number (sum (map (evalNum . evalExpr env) xs))
        Operator '-' ->
          Number (foldl (-)
                  (evalNum $ evalExpr env (head xs))
                  (map (evalNum . evalExpr env) (tail xs)))
        Symbol symbol ->
          case symbol of
            -- Primitive procedures
            "=" -> equal (evalExpr env (head xs))
                         (evalExpr env (head (tail xs)))
            "or" -> Wiz.EvalApply.or (map (evalExpr env) xs)
            "not" -> Wiz.EvalApply.not (evalExpr env (head xs))
            "nil?" -> nil (evalExpr env (head xs))
            "pair?" -> pair (evalExpr env (head xs))
            "car" -> car (evalExpr env (head xs))
            "cdr" -> cdr (evalExpr env (head xs))
            "cons" -> cons env (head xs) (head (tail xs))
            _ -> apply env (envLookup symbol env) xs
        _ -> List (map (evalExpr env) exprs)

    Quote expr -> expr
    If test consequent alternate ->
      (if (evalBool $ evalExpr env test) then
         (evalExpr env consequent)
       else (evalExpr env alternate))

    Lambda formals body -> expression -- returns itself


-- Apply

-- "A procedure is, slightly simplified,
-- an abstraction of an expression over objects."

apply :: Environment -> Expression -> [Expression] -> Expression
-- apply env _ _ | trace ("apply in\n" ++ show env) False = undefined
apply env (Lambda (Formals formals) body) arguments =
  evalExpr env' body
  where env' = encloseEnvironment env
                 (extendEnvironment emptyEnv (zip formals evaledArguments))
        evaledArguments = map (evalExpr env) arguments
apply _ _ _ = undefined
