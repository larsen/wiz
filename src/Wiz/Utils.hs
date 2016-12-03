module Wiz.Utils (
  emptyEnv
) where

import Wiz.Types
import Wiz.Environment
import Wiz.EvalApply

import qualified Data.Map as Map

emptyEnv :: Environment
emptyEnv = Environment (Map.fromList [])

