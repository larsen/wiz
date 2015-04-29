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

loadProgram :: String -> IO Environment
loadProgram file = do
  program <- readFile file
  let res = parse pProgram "(source)" program
  case res of
    Left err -> do
      return $ emptyEnv
    Right p -> do
      return $ runProgram emptyEnv p
  where
    runProgram env (Program (x:xs)) =
      let (env', res) = eval x env
      in case res of
        Just res -> runProgram env' (Program xs)
        Nothing  -> runProgram env' (Program xs) -- ???
    runProgram env (Program []) = env
