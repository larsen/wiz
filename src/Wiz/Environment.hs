module Wiz.Environment (
  emptyEnv,
  envLookup,
  extendEnvironment,
  encloseEnvironment,
  BoundValue( Value, Closure ),
  Environment( Environment )
) where

import Wiz.Types
import Data.Map hiding (map)
import Data.Maybe (fromMaybe)

type Closure = (Expression, Environment)
data BoundValue = Value Expression
                | Closure Closure
                  deriving (Show, Eq)

type Bindings = Map String BoundValue
  
data Environment = Environment { env :: Bindings
                               , parent :: Maybe Environment
                               } deriving (Eq)

instance Show Environment where
  show (Environment env parent) =
    unlines (map showPair $ toList env) ++ "\n"
    where showPair (k, v) = show k ++ "\t->\t" ++ show v

emptyEnv :: Environment
emptyEnv = Environment (fromList []) Nothing

encloseEnvironment :: Environment -> Environment -> Environment
encloseEnvironment parentEnv childEnv = Environment (env childEnv) (Just parentEnv)

extendEnvironment :: Environment -> Bindings -> Environment
extendEnvironment (Environment env parent) bindings =
  Environment (union bindings env) parent

envLookup :: String -> Environment -> BoundValue
-- envLookup symbol env
--   | trace ("envlookup " ++ show symbol ++ " in\n" ++ show env) False = undefined

envLookup symbol (Environment env parent) =
  fromMaybe
    (case parent of
      Just p -> envLookup symbol p
      Nothing -> error ("Unbound symbol " ++ show symbol))
    res
  where res = Data.Map.lookup symbol env
