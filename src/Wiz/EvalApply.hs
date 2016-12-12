module Wiz.EvalApply (
    eval
  ) where

import Wiz.Types
import Wiz.Environment
import qualified Data.Map as Map
import Data.Maybe
import Text.Printf
import Debug.Trace

eval :: Form -> Environment -> (Environment, Maybe Value)
eval (FExpr (Definition sym expr)) env =
  (evalDefinition (E (Definition sym expr)) env, Nothing)
eval (FExpr (SetInstruction symbol expr)) env =
  (evalSetInstruction symbol expr env, Nothing)
eval (FExpr expr) env = (env, Just $ evalExpr env expr)

-- TODO refactor
evalSetInstruction :: String -> Expression -> Environment -> Environment
evalSetInstruction symbol expr env =
  changeValue env symbol (evalExpr env expr)

evalDefinition :: Value -> Environment -> Environment
evalDefinition (E (Definition symbol expr)) env =
  changeValue env symbol (evalExpr env expr)

evalNum :: Value -> Integer
evalNum (E (Number n)) = n
evalNum e = error $ "evalNum " ++ show e

evalBool :: Value -> Bool
evalBool (E (Boolean b)) = b
evalBool e = error $ "evalBool " ++ show e

equal :: Value -> Value -> Expression
equal x y | x == y = Boolean True
          | otherwise = Boolean False

car :: Value -> Expression
car (E (List (x:_))) = x
car (E (List [])) = List []
car _ = error "car applied to non list expression!"

cdr :: Value -> Expression
cdr (E (List (_:xs))) = List xs
cdr (E (List [])) = List []
cdr _ = error "cdr applied to non list expression!"

cons :: Environment -> Expression -> Expression -> Expression
cons env x (List []) = List [x]
cons env x y = List [x, y]

nil :: Value -> Expression
nil (E (List [])) = Boolean True
nil _ = Boolean False

not :: Value -> Expression
not (E (Boolean False)) = Boolean True
not _ = Boolean False

or :: [Value] -> Expression
or = Boolean . any evalBool

pair :: Value -> Expression
pair (E (List (x:_))) = Boolean True
pair _ = Boolean False

evalExpr :: Environment -> Expression -> Value
-- evalExpr env expr
--   | trace ("evalExpr " ++ show expr ++ " in\n" ++ show env) False = undefined
evalExpr env (Number n)                     = E $ Number n
evalExpr env (Boolean b)                    = E $ Boolean b
evalExpr env (Quote expression)             = E expression
evalExpr env (Lambda formals body)          = C (Lambda formals body, env)
evalExpr env (If test consequent alternate) = if evalBool $ evalExpr env test then
                                                evalExpr env consequent
                                              else evalExpr env alternate

evalExpr env (Symbol s) =
  case envLookup s env of
    E e -> evalExpr env e
    C (c, env') -> evalExpr env' c

evalExpr env (List [])           = E $ List []
evalExpr env (List exprs@(x:xs)) =
  case x of
    Operator "<" -> E (Boolean ((evalExpr env (head xs)) <
                                 (evalExpr env (head (tail xs)))))
    Operator ">" -> E (Boolean ((evalExpr env (head xs)) >
                                 (evalExpr env (head (tail xs)))))
    Operator "*" -> E (Number (product (map (evalNum . evalExpr env) xs)))
    Operator "+" -> E (Number (sum (map (evalNum . evalExpr env) xs)))
    Operator "-" -> E (Number (foldl (-)
                               (evalNum $ evalExpr env (head xs))
                               (map (evalNum . evalExpr env) (tail xs))))
    Symbol symbol ->
      case symbol of
        -- Primitive procedures
        "=" -> E (equal (evalExpr env (head xs))
                   (evalExpr env (head (tail xs))))
        "or" -> E (Wiz.EvalApply.or (map (evalExpr env) xs))
        "not" -> E (Wiz.EvalApply.not (evalExpr env (head xs)))
        "nil?" -> E (nil (evalExpr env (head xs)))
        "pair?" -> E (pair (evalExpr env (head xs)))
        "car" -> E (car (evalExpr env (head xs)))
        "cdr" -> E (cdr (evalExpr env (head xs)))
        "cons" -> E (cons env (head xs) (head (tail xs)))
        "let" -> evalLet env (head xs) (last xs)
        _ -> apply env (envLookup symbol env) xs
    _ -> E (List exprs)

symbolToString :: Expression -> String
symbolToString (Symbol s) = s

listToList (List l) = l

evalLet env (List bindings) body = evalExpr env' body
  where 
    env' = encloseEnvironment env
      (extendEnvironment env $ Map.fromList
       (zip bindingsNames (map E bindingsExpressions)))
    bindingsNames = map ((symbolToString . head) . listToList) bindings
    bindingsExpressions = map (last . listToList) bindings

-- Apply

-- "A procedure is, slightly simplified,
-- an abstraction of an expression over objects."

apply :: Environment -> Value -> [Expression] -> Value
-- apply env _ _ | trace ("apply in\n" ++ show env) False = undefined

apply env (C (Lambda (Formals formals) body, env')) arguments = 
  evalExpr env'' body
  where env'' = composeEnvironments [ extendEnvironment
                                      env' $ Map.fromList
                                       (zip formals evaledArguments)
                                    , env
                                    ] 
        evaledArguments = map (evalExpr env) arguments
apply _ _ _ = undefined
