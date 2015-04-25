module Wiz.EvalApply (
    eval
  ) where

import Wiz.Types
import qualified Data.Map as Map
import qualified Data.List as L
import Text.Printf
import Debug.Trace

envLookup :: String -> Environment -> Expression
-- envLookup symbol env
--   | trace ("envlookup " ++ show symbol ++ " in\n" ++ show env) False = undefined

-- FIXME dovrebbe restituire un errore
envLookup symbol (Environment env) =
  case res of
    Just res -> res
    Nothing -> undefined
  where res = Map.lookup symbol env

eval :: Form -> Environment -> (Environment, Maybe Expression)
eval form env =
  case form of
    FDef def -> (evalDefinition def env, Nothing)
    FExpr expr -> (env, Just $ evalExpr env expr)

evalDefinition :: Definition -> Environment -> Environment
evalDefinition (Definition symbol expr) (Environment env) =
  Environment (Map.insert symbol expr env)

-- FIXME ugly
evalInBooleanContext :: Expression -> Bool
-- evalInBooleanContext n | trace ("evalInBooleanContext " ++ show n) False = undefined
evalInBooleanContext (Number 0) = False
evalInBooleanContext _ = True

evalNumeric :: [Expression] -> [Integer]
-- evalNumeric n | trace ("evalNumeric " ++ show n) False = undefined
evalNumeric [] = []
evalNumeric ((Number x):xs) = x : evalNumeric xs

evalNumericElem :: Expression -> Integer
evalNumericElem (Number n) = n
evalNumericElem _ = undefined

evalExpr :: Environment -> Expression -> Expression
evalExpr env expr
  | trace ("evalExpr " ++ show expr ++ " in\n" ++ show env) False = undefined
evalExpr env expression =
  case expression of
    Number n -> Number n
    Symbol s -> evalExpr env (envLookup s env)
    
    Operator '*' exprs -> Number (foldl (*) 1 (evalNumeric (map (evalExpr env) exprs)))
    Operator '+' exprs -> Number (foldl (+) 0 (evalNumeric (map (evalExpr env) exprs)))
    Operator '-' (exp:exps) ->
      Number (foldl (-)
              (evalNumericElem $ evalExpr env exp)
              (evalNumeric (map (evalExpr env) exps)))

    List exprs -> List (map (evalExpr env) exprs)
    Quote expr -> expr
    If test consequent alternate ->
      (if (evalInBooleanContext $ evalExpr env test) then
         (evalExpr env consequent)
       else (evalExpr env alternate))

    Lambda formals expression -> Number 0 -- not sure about this return value
    LambdaApply symbol exprs -> apply env (envLookup symbol env) exprs


-- Apply

-- "A procedure is, slightly simplified,
-- an abstraction of an expression over objects."

apply :: Environment -> Expression -> [Expression] -> Expression
-- apply env _ _ | trace ("apply in\n" ++ show env) False = undefined
apply env (Lambda (Formals formals) expr) arguments =
  evalExpr env' expr
  where env' = extendEnvironment env (zip formals evaledArguments)
        -- FIXME non sto valutando le espressioni!
        extendEnvironment (Environment env) newElems =
          Environment (Map.union (Map.fromList newElems) env)
        evaledArguments = map (evalExpr env) arguments
apply _ _ _ = undefined
