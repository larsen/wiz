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
eval (FExpr (Definition sym expr)) env =
  (evalDefinition (Value (Definition sym expr)) env, Nothing)
eval (FExpr expr) env = (env, Just $ evalExpr env (Value expr))

evalDefinition :: BoundValue -> Environment -> Environment
evalDefinition (Value (Definition symbol expr)) env = extendEnvironment env [(symbol, Value expr)]

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
cons env x (List []) = List [evalExpr env (Value x)]
cons env x y = List [evalExpr env (Value x), evalExpr env (Value y)]

nil :: Expression -> Expression
nil (List []) = Boolean True
nil _ = Boolean False

not :: Expression -> Expression
not (Boolean False) = Boolean True
not _ = Boolean False

or :: [Expression] -> Expression
or = Boolean . any evalBool

pair :: Expression -> Expression
pair (List (x:_)) = Boolean True
pair _ = Boolean False

evalExpr :: Environment -> BoundValue -> Expression
-- evalExpr env expr
--   | trace ("evalExpr " ++ show expr ++ " in\n" ++ show env) False = undefined
evalExpr env (Value expression) =
  case expression of
    Number n -> Number n
    Boolean b -> Boolean b
    Symbol s -> evalExpr env (envLookup s env)
    
    List [] -> expression
    List exprs@(x:xs) ->
      case x of
        Operator '*' -> Number (product (map (evalNum . evalExpr env . Value) xs))
        Operator '+' -> Number (sum (map (evalNum . evalExpr env . Value) xs))
        Operator '-' ->
          Number (foldl (-)
                  (evalNum $ evalExpr env (Value (head xs)))
                  (map (evalNum . evalExpr env . Value) (tail xs)))
        Symbol symbol ->
          case symbol of
            -- Primitive procedures
            "=" -> equal (evalExpr env (Value (head xs)))
                         (evalExpr env (Value (head (tail xs))))
            "or" -> Wiz.EvalApply.or (map ((evalExpr env) . Value) xs)
            "not" -> Wiz.EvalApply.not (evalExpr env (Value (head xs)))
            "nil?" -> nil (evalExpr env (Value (head xs)))
            "pair?" -> pair (evalExpr env (Value (head xs)))
            "car" -> car (evalExpr env (Value (head xs)))
            "cdr" -> cdr (evalExpr env (Value (head xs)))
            "cons" -> cons env (head xs) (head (tail xs))
            "let" -> evalLet env (head xs) (Value (last xs))
            _ -> apply env (envLookup symbol env) xs
        _ -> List (map (evalExpr env . Value) exprs)

    Quote expr -> expr
    If test consequent alternate ->
      if evalBool $ evalExpr env (Value test) then
        evalExpr env (Value consequent)
       else evalExpr env (Value alternate)

    Lambda formals body -> expression -- returns itself

symbolToString :: Expression -> String
symbolToString (Symbol s) = s

listToList (List l) = l

evalLet env (List bindings) body = evalExpr env' body
  where
    env' = encloseEnvironment env (extendEnvironment emptyEnv
                                    (zip bindingsNames (map Value bindingsExpressions)))
    bindingsNames = map ((symbolToString . head) . listToList) bindings
    bindingsExpressions = map (last . listToList) bindings

-- Apply

-- "A procedure is, slightly simplified,
-- an abstraction of an expression over objects."

apply :: Environment -> BoundValue -> [Expression] -> Expression
-- apply env _ _ | trace ("apply in\n" ++ show env) False = undefined
apply env (Value (Lambda (Formals formals) body)) arguments =
  evalExpr env' (Value body)
  where env' = encloseEnvironment env
                 (extendEnvironment emptyEnv (zip formals (map Value evaledArguments)))
        evaledArguments = map (evalExpr env . Value) arguments
apply _ _ _ = undefined
