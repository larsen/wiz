module Main where

import Wiz.Types
import Wiz.Parser
import Wiz.EvalApply
import Text.Parsec (parse)
import Text.Printf
import System.Console.Haskeline
import qualified Data.Map as Map

main :: IO ()
main = runInputT defaultSettings (loop env)
   where
       env = Environment (Map.fromList
                          -- A couple of functions for tests
                          [("fact",Lambda (Formals ["n"])
                                   (If (Symbol "n")
                                    (Operator '*'
                                     [Symbol "n",
                                      LambdaApply "fact" [Operator '-' [Symbol "n",Number 1]]])
                                    (Number 1))),
                           ("sqr",Lambda (Formals ["n"])
                                  (Operator '*' [Symbol "n", Symbol "n"]))])
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
                       -- Just result -> outputStrLn $ printf "%s\n%s\n" (show result) (show env')
                       Just result -> outputStrLn $ printf "%s\n" (show result)
                       Nothing     -> outputStrLn $ printf "\n" -- (show env')
                     loop env'
