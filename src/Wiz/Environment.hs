module Wiz.Environment (
  emptyEnv,
  envLookup,
  extendEnvironment,
  encloseEnvironment,
  addEnvironment,
  composeEnvironments,
  changeValue,
  Value( E, C ),
  Environment( Environment )
) where

import Wiz.Types
import Data.Map hiding (map, foldr)
import Data.Maybe (fromMaybe)

type Closure = (Expression, Environment)
data Value = E Expression
           | C Closure
             deriving (Eq)

instance Show Value where
  show (E expression) = show expression
  show (C (expression, env)) = show expression ++ "\nin " ++ show env

type Bindings = Map String Value
  
data Environment = Environment { env :: Bindings
                               , parent :: Maybe Environment
                               } deriving (Eq)

instance Show Environment where
  show environment = recursiveShow 0 environment
    where recursiveShow level (Environment e parent) =
            "ENV: " ++ concat (replicate level "\t") ++ unlines (map showPair $ toList e) ++ case parent of
                                             Just p -> "\n" ++ recursiveShow (level +1) p
                                             Nothing -> "\nNO PARENT"
          showPair (k, v) = show k ++ "\t->\t" ++ show v

emptyEnv :: Environment
emptyEnv = Environment (fromList []) Nothing

addEnvironment :: Environment -> Environment -> Environment
addEnvironment e1 e2 = Environment { env = env e1, parent = Just e2 }

composeEnvironments :: [Environment] -> Environment
composeEnvironments es = foldr addEnvironment emptyEnv es
  
encloseEnvironment :: Environment -> Environment -> Environment
encloseEnvironment parentEnv childEnv = composeEnvironments [childEnv, parentEnv]

extendEnvironment :: Environment -> Bindings -> Environment
extendEnvironment (Environment env parent) bindings =
  Environment (union bindings env) parent

envLookup :: String -> Environment -> Value
-- envLookup symbol env
--   | trace ("envlookup " ++ show symbol ++ " in\n" ++ show env) False = undefined

envLookup symbol (Environment env parent) =
  fromMaybe
    (case parent of
      Just p -> envLookup symbol p
      Nothing -> error ("Unbound symbol " ++ show symbol))
    res
  where res = Data.Map.lookup symbol env

changeValue :: Environment -> String -> Value -> Environment
changeValue e symbol value = 
  Environment { env = insert symbol value (env e)
              , parent = parent e
              }
