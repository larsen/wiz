module Wiz.Utils (
  loadProgram
) where

import Wiz.Types
import Wiz.EvalApply
import Wiz.Parser

import Text.Parsec (parse)
import qualified Data.Map as Map

emptyEnv :: Environment
emptyEnv = Environment (Map.fromList [])

