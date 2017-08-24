module Wiz.TestUtils where

import           Test.Hspec
import           Text.Parsec (parse)
import qualified Wiz.EvalApply as W
import           Wiz.Parser
import           Wiz.Types
import           Wiz.Environment

import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec.Error (
  ParseError,
  Message,
  errorMessages,
  messageEq )

parseForm :: String -> IO Form
parseForm str = do
  let res = parse pForm "(source)" str
  case res of
    Left err -> error "Something gone wrong!"
    Right f  -> return f

testForm :: [String] -> String -> String -> IO ()
testForm programFiles form expectedForm = do
  env <- foldM (\e pf ->
                  loadProgram pf >>= (W.runProgram e . fromMaybe (Program [])))
         emptyEnv programFiles
  expr <- parseForm form
  expectedExpr <- parseForm expectedForm
  expectedResults <- W.eval expectedExpr env
  results <- W.eval expr env
  snd results `shouldBe` snd expectedResults
