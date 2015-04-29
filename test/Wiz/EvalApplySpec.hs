module Wiz.EvalApplySpec where

import qualified Wiz.EvalApply as W
import Wiz.Types
import Test.Hspec
import qualified Data.Map as Map


spec =
  
  describe "eval" $ do
    it "eval numeric expressions as integers" $ do
      W.eval (FExpr (Number 1)) env `shouldBe` (env, Just (Number 1))

    it "eval simple arithmetic operations" $ do
      W.eval (FExpr (List [Operator '+', Number 2, Number 2])) env
        `shouldBe` (env, Just (Number 4))

    it "eval function calls /sqr" $ do
      W.eval (FExpr (List [Symbol "sqr", Number 10])) env
        `shouldBe` (env, Just (Number 100))

    it "eval recursive function calls /fact" $ do
      W.eval (FExpr (List [Symbol "fact", Number 10])) env
        `shouldBe` (env, Just (Number 3628800))

      
  where env = Environment (Map.fromList
                           -- A couple of functions for tests
                           [("fact", Lambda (Formals ["n"])
                                    (If (List [Symbol "=", Symbol "n", Number 0])
                                     (Number 1) 
                                     (List [Operator '*',
                                            Symbol "n",
                                            (List [Symbol "fact",
                                                   (List [Operator '-', Symbol "n", Number 1])])
                                           ]))),
                           ("sqr",Lambda (Formals ["n"])
                                  (List [Operator '*', Symbol "n", Symbol "n"]))]) 
