module Main where

import Wiz.Types
import Wiz.Parser
import Wiz.EvalApply

import Text.Parsec (parse)
import Text.Printf
import System.Console.Haskeline
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

main :: IO ()
main = do
   env <- loadProgram "init.scm"
   runInputT defaultSettings (loop env)
   where
       loop :: Environment -> InputT IO ()
       loop env = do
           input <- getInputLine "λ> "
           case input of
               Nothing -> return ()
               Just input -> do
                 let res = parse pForm "(source)" input
                 case res of
                   Left err -> do
                     outputStrLn "Error!"
                     loop env
                   Right form -> do
                     let (env', result) = eval form env
                     case result of
                       Just result -> outputStrLn $ printf "%s" (show result)
                       Nothing     -> outputStrLn $ printf "\n"
                     loop env'
