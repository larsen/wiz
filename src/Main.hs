module Main where

import Wiz.Types
import Wiz.Parser
import Wiz.EvalApply

import Text.Parsec (parse)
import Text.Printf
import System.Console.Haskeline
import qualified Data.Map as Map

initialEnv :: Environment
initialEnv =  Environment (Map.fromList
                          -- A couple of functions for tests
                          [("fact",Lambda (Formals ["n"])
                                   (If (Symbol "n")
                                    (List [Operator '*',
                                           Symbol "n",
                                           (List [Symbol "fact",
                                                  (List [Operator '-', Symbol "n", Number 1])])
                                     ])
                                    (Number 1))),
                           ("sqr",Lambda (Formals ["n"])
                                  (List [Operator '*', Symbol "n", Symbol "n"]))]) 

main :: IO ()
main = runInputT defaultSettings (loop env)
   where
       env = initialEnv
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
                       Just result -> outputStrLn $ printf "%s\n" (show result)
                       Nothing     -> outputStrLn $ printf "\n"
                     loop env'
