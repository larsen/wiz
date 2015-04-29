module Wiz.EvalApplySpec where

import qualified Wiz.EvalApply as W
import Wiz.Types
import Wiz.Utils
import Test.Hspec
import qualified Data.Map as Map

spec =
  
  describe "eval" $ do

    it "eval numeric expressions as integers" $ do
      env <- loadProgram "init.scm"
      W.eval (FExpr (Number 1)) env `shouldBe` (env, Just (Number 1))

    it "eval simple arithmetic operations" $ do
      env <- loadProgram "init.scm"
      W.eval (FExpr (List [Operator '+', Number 2, Number 2])) env
        `shouldBe` (env, Just (Number 4))

    it "eval function calls /squqre" $ do
      env <- loadProgram "test/square.scm"
      W.eval (FExpr (List [Symbol "square", Number 10])) env
        `shouldBe` (env, Just (Number 100))

    it "eval recursive function calls /fact" $ do
      env <- loadProgram "init.scm"
      W.eval (FExpr (List [Symbol "fact", Number 10])) env
        `shouldBe` (env, Just (Number 3628800))
