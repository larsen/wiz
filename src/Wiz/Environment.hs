module Wiz.Environment (
  envLookup,
  extendEnvironment,
  Environment( Environment )
) where

import Wiz.Types
import qualified Data.Map as Map
import qualified Data.List as L

data Environment = Environment (Map.Map String Expression)
                   deriving (Eq)

instance Show Environment where
  show (Environment env) =
    L.unlines (map showPair (Map.toList env)) ++ "\n"
    where showPair (k, v) = show k ++ "\t->\t" ++ show v

type Binding = (String, Expression)

extendEnvironment :: Environment -> [Binding] -> Environment
extendEnvironment (Environment env) bindings =
  Environment (Map.union (Map.fromList bindings) env)

envLookup :: String -> Environment -> Expression
-- envLookup symbol env
--   | trace ("envlookup " ++ show symbol ++ " in\n" ++ show env) False = undefined

envLookup symbol (Environment env) =
  case res of
    Just res -> res
    Nothing -> error ("Unbound symbol " ++ show symbol)
  where res = Map.lookup symbol env
