module Wiz.Environment (
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
