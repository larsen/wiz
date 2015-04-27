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
                                   (If (List [Symbol "=", Symbol "n", Number 0])
                                    (Number 1)
                                    (List [Operator '*',
                                           Symbol "n",
                                           (List [Symbol "fact",
                                                  (List [Operator '-', Symbol "n", Number 1])])
                                     ]))),
                           ("map", Lambda (Formals ["f","lst"])
                                   (If (List [Symbol "nil?",Symbol "lst"])
                                    (Quote (List []))
                                    (List [Symbol "cons",
                                           List [Symbol "f",List [Symbol "car",Symbol "lst"]],
                                           List [Symbol "map",Symbol "f",List [Symbol "cdr",Symbol "lst"]]]))),
                           ("else", (Boolean True)),
                           ("cond", Lambda (Formals ["lst"])
                                    (If (List [Symbol "nil?",Symbol "lst"])
                                     (Quote (List []))
                                     (If (List [Symbol "car",Symbol "lst"])
                                      (List [Symbol "car",List [Symbol "cdr",Symbol "lst"]])
                                      (List [Symbol "cond",List [Symbol "cdr",Symbol "lst"]])))),
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
                       Just result -> outputStrLn $ printf "%s" (show result)
                       Nothing     -> outputStrLn $ printf "\n"
                     loop env'
