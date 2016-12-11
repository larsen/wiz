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
  (evalDefinition (E (Definition sym expr)) env, Nothing)
eval (FExpr expr) env = (env, Just $ evalExpr env (E expr))


evalDefinition :: Value -> Environment -> Environment
evalDefinition (E (Definition symbol expr)) env =
  extendEnvironment env $ Map.fromList [(symbol, E $ evalExpr env (E expr))]

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
cons env x (List []) = List [evalExpr env (E x)]
cons env x y = List [evalExpr env (E x), evalExpr env (E y)]

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

evalExpr :: Environment -> Value -> Expression
-- evalExpr env expr
--   | trace ("evalExpr " ++ show expr ++ " in\n" ++ show env) False = undefined
evalExpr env (E expression) =
  case expression of
    Number n -> Number n
    Boolean b -> Boolean b
    Symbol s -> evalExpr env (envLookup s env)
    
    List [] -> expression
    List exprs@(x:xs) ->
      case x of
        Operator '*' -> Number (product (map (evalNum . evalExpr env . E) xs))
        Operator '+' -> Number (sum (map (evalNum . evalExpr env . E) xs))
        Operator '-' ->
          Number (foldl (-)
                  (evalNum $ evalExpr env (E (head xs)))
                  (map (evalNum . evalExpr env . E) (tail xs)))
        Symbol symbol ->
          case symbol of
            -- Primitive procedures
            "=" -> equal (evalExpr env (E (head xs)))
                         (evalExpr env (E (head (tail xs))))
            "or" -> Wiz.EvalApply.or (map (evalExpr env . E) xs)
            "not" -> Wiz.EvalApply.not (evalExpr env (E (head xs)))
            "nil?" -> nil (evalExpr env (E (head xs)))
            "pair?" -> pair (evalExpr env (E (head xs)))
            "car" -> car (evalExpr env (E (head xs)))
            "cdr" -> cdr (evalExpr env (E (head xs)))
            "cons" -> cons env (head xs) (head (tail xs))
            "let" -> evalLet env (head xs) (E (last xs))
            _ -> apply env (envLookup symbol env) xs
        _ -> List (map (evalExpr env . E) exprs)

    Quote expr -> expr
    If test consequent alternate ->
      if evalBool $ evalExpr env (E test) then
        evalExpr env (E consequent)
       else evalExpr env (E alternate)

    Lambda formals body -> expression -- returns itself

symbolToString :: Expression -> String
symbolToString (Symbol s) = s

listToList (List l) = l

evalLet env (List bindings) body = evalExpr env' body
  where
    env' = encloseEnvironment env (extendEnvironment emptyEnv $
                                    Map.fromList (zip bindingsNames (map E bindingsExpressions)))
    bindingsNames = map ((symbolToString . head) . listToList) bindings
    bindingsExpressions = map (last . listToList) bindings

-- Apply

-- "A procedure is, slightly simplified,
-- an abstraction of an expression over objects."

apply :: Environment -> Value -> [Expression] -> Expression
-- apply env _ _ | trace ("apply in\n" ++ show env) False = undefined
apply env (E (Lambda (Formals formals) body)) arguments =
  evalExpr env' (E body)
  where env' = encloseEnvironment env
                 (extendEnvironment emptyEnv $ Map.fromList (zip formals (map E evaledArguments)))
        evaledArguments = map (evalExpr env . E) arguments
apply _ _ _ = undefined
