module Main where

import Wiz.Types
import Wiz.Environment
import Wiz.Parser
import Wiz.EvalApply
import Wiz.Utils

import Text.Parsec (parse)
import Text.Printf
import System.Console.Haskeline

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
