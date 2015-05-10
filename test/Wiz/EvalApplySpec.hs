module Wiz.EvalApplySpec where

import qualified Wiz.EvalApply as W
import Wiz.Types
import Wiz.Utils
import Wiz.Parser
import Test.Hspec
import Text.Parsec (parse)
import qualified Data.Map as Map


-- parseForm :: String -> IO Form
parseForm str = do
  let res = (parse pForm "(source)" str)
  case res of
    Left err -> error "Something gone wrong!"
    Right f  -> return f

spec =
  
  describe "eval" $ do

    it "eval numeric expressions as integers" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "1"
      W.eval expr env `shouldBe` (env, Just (Number 1))

    it "eval simple arithmetic operations" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(+ 2 2)"
      W.eval expr env `shouldBe` (env, Just (Number 4))

    it "eval function calls /squqre" $ do
      env <- loadProgram "test/square.scm"
      expr <- parseForm "(square 10)"
      W.eval expr env `shouldBe` (env, Just (Number 100))

    it "eval recursive function calls /fact" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(fact 10)"
      W.eval expr env `shouldBe` (env, Just (Number 3628800))

    it "eval another recursive function calls /length" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(length '(1 2 3))"
      W.eval expr env `shouldBe` (env, Just (Number 3))
