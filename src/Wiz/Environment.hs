module Wiz.Environment (
  emptyEnv,
  envLookup,
  extendEnvironment,
  encloseEnvironment,
  composeEnvironments,
  Value( E, C ),
  Environment( Environment )
) where

import Wiz.Types
import Data.Map hiding (map)
import Data.Maybe (fromMaybe)

type Closure = (Expression, Environment)
data Value = E Expression
           | C Closure
             deriving (Show, Eq)

type Bindings = Map String Value
  
data Environment = Environment { env :: Bindings
                               , parent :: Maybe Environment
                               } deriving (Eq)

instance Show Environment where
  show (Environment env parent) =
    unlines (map showPair $ toList env) ++ "\n"
    where showPair (k, v) = show k ++ "\t->\t" ++ show v

emptyEnv :: Environment
emptyEnv = Environment (fromList []) Nothing

composeEnvironments :: [Environment] -> Environment
composeEnvironments (e:[]) = Environment (env e) (Just emptyEnv)
composeEnvironments (e:es) = Environment (env (composeEnvironments es)) (Just e)

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
