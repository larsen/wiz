module Wiz.EvalApply (
    eval,
    runProgram
  ) where

import Wiz.Types
import Wiz.Environment
import Wiz.Parser
import qualified Data.Map as Map
import Data.Maybe
import Text.Printf
import Debug.Trace

runProgram :: Environment -> Program -> IO Environment
runProgram env (Program (x:xs)) = do
  (env', res) <- eval x env
  case res of
    Just res -> runProgram env' (Program xs)
    Nothing  -> runProgram env' (Program xs) -- ???
runProgram env (Program []) = return env

eval :: Form -> Environment -> IO (Environment, Maybe Value)
eval (FExpr (Definition sym expr)) env =
  return (evalDefinition (E (Definition sym expr)) env, Nothing)
eval (FExpr (SetInstruction symbol expr)) env =
  return (evalSetInstruction symbol expr env, Nothing)
eval (FExpr (SetCarInstruction symbol expr)) env =
  return (evalSetCarInstruction symbol expr env, Nothing)
eval (FExpr (SetCdrInstruction symbol expr)) env =
  return (evalSetCdrInstruction symbol expr env, Nothing)
eval (FExpr (List [Symbol "load", String file])) env = do
  prg <- loadProgram file
  env' <- runProgram env $ fromMaybe (Program []) prg
  return (env', Nothing)
eval (FExpr (List [Symbol "display", expr])) env = do
  result <- return $ evalExpr env expr
  printf "%s\n" $ show result
  return (env, Nothing)
eval (FExpr expr) env = return (env, Just $ evalExpr env expr)

-- TODO refactor
evalSetInstruction :: String -> Expression -> Environment -> Environment
evalSetInstruction symbol expr env =
  changeValue env symbol (evalExpr env expr)

evalSetCarInstruction :: String -> Expression -> Environment -> Environment
evalSetCarInstruction symbol expr env =
  case symbolValue of
    (E (List (x:xs))) -> changeValue env symbol (E $ List (expr:xs))
    _ -> error "set-car! applied to non-list value"
  where symbolValue = envLookup symbol env

evalSetCdrInstruction :: String -> Expression -> Environment -> Environment
evalSetCdrInstruction symbol expr env =
  case symbolValue of
    (E (List (x:xs))) -> changeValue env symbol $
      evalExpr env (cons (E x) (evalExpr env expr))
    _ -> error "set-cdr! applied to non-list value"
  where symbolValue = envLookup symbol env

evalDefinition :: Value -> Environment -> Environment
evalDefinition (E (Definition symbol expr)) env =
  changeValue env symbol (evalExpr env expr)

evalNum :: Value -> Double
evalNum (E (Number n)) = n
evalNum e = error $ "evalNum " ++ show e

evalBool :: Value -> Bool
-- evalBool v | trace ("evalBool " ++ show v) False = undefined
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

cons :: Value -> Value -> Expression
-- cons v1 v2
--   | trace ("-> cons v1:" ++ show v1 ++ ", v2:" ++ show v2) False = undefined
cons (E x) (E (List ys)) = List $ x:ys
cons (E x) (E e) = List [x,e]

list :: [Expression] -> Expression
list = List

nil :: Value -> Expression
nil (E (List [])) = Boolean True
nil _ = Boolean False

not :: Value -> Expression
not (E (Boolean False)) = Boolean True
not _ = Boolean False

or :: [Value] -> Expression
or = Boolean . any evalBool

and :: [Value] -> Expression
and = Boolean . all evalBool

pair :: Value -> Expression
pair (E (List (x:_))) = Boolean True
pair _ = Boolean False

compareList :: (Ord a) => (a -> a -> Bool) -> [a] -> Bool
compareList _ [] = True
compareList f list = Prelude.and $ zipWith f list (tail list)

evalExpr :: Environment -> Expression -> Value
evalExpr env expr
  | trace ("evalExpr " ++ show expr) False = undefined
evalExpr env (Number n)  = E $ Number n
evalExpr env (String s)  = E $ String s
evalExpr env (Boolean b) = E $ Boolean b

-- "quoted data is first rewritten into calls to the list construction
-- functions before ordinary evaluation proceeds."
-- http://www.r6rs.org/final/r6rs.pdf


-- FIXME!!!
evalExpr env (Quote (List lst)) = evalExpr env $ List [Symbol "list", List lst]
-- evalExpr env (Quote (List lst)) = E $ List [Symbol "list", List lst]
-- evalExpr env (Quote (List (x:xs))) = evalExpr env $ List [Symbol "cons", x, List xs]
evalExpr env (Quote expression) = E expression

evalExpr env (Lambda formals body)          = C (Lambda formals body, env)
evalExpr env (Cond (Clause test consequent:cls)) =
  if evalBool $ evalExpr env test then evalExpr env consequent
  else evalExpr env (Cond cls)
evalExpr env (Symbol s) =
  case envLookup s env of
    E e -> evalExpr env e
    C (c, env') -> evalExpr env' c

evalExpr env (List [])           = E $ List []
evalExpr env (List exprs@(x:xs)) =
  case x of
    Operator "<"  -> E (Boolean $ compareList (<)  (map (evalNum . evalExpr env) xs))
    Operator ">"  -> E (Boolean $ compareList (>)  (map (evalNum . evalExpr env) xs))
    Operator "<=" -> E (Boolean $ compareList (<=) (map (evalNum . evalExpr env) xs))
    Operator ">=" -> E (Boolean $ compareList (>=) (map (evalNum . evalExpr env) xs))
    Operator "*" -> E (Number (product (map (evalNum . evalExpr env) xs)))
    Operator "/" -> E (Number $ dividend / divisor)
                    where dividend = evalNum $ evalExpr env (head xs)
                          divisor = evalNum $ evalExpr env (head (tail xs))
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
        "and" -> E (Wiz.EvalApply.and (map (evalExpr env) xs))
        "not" -> E (Wiz.EvalApply.not (evalExpr env (head xs)))
        "null?" -> E (nil (evalExpr env (head xs)))
        "pair?" -> E (pair (evalExpr env (head xs)))
        "car" -> E (car (evalExpr env (head xs)))
        "cdr" -> E (cdr (evalExpr env (head xs)))
        "cons" -> E (cons (evalExpr env (head xs))
                     (evalExpr env (head (tail xs))))
        "list" -> E $ head xs
        "let" -> evalLet env (head xs) (last xs)
        _ -> apply env (envLookup symbol env) xs
    _ -> E (List exprs) -- cons env (evalExpr env x) (E (List xs))

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
apply env _ _ | trace ("apply in\n" ++ show env) False = undefined

apply env (C (Lambda (Formals formals) body, env')) arguments = 
  evalExpr env'' body
  where env'' = composeEnvironments [ extendEnvironment
                                      env' $ Map.fromList
                                       (zip formals evaledArguments)
                                    , env
                                    ] 
        evaledArguments = map (evalExpr env) arguments
apply _ _ _ = undefined
