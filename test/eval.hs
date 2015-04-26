module EvalApplySpec where

import Wiz.EvalApply
import Wiz.Types
import Test.Hspec
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
  
  describe "eval" $ do
    it "eval numeric expressions as integers" $ do
      eval (FExpr (Number 1)) env `shouldBe` (env, Just (Number 1))

    it "eval function calls /sqr" $ do
      eval (FExpr (List [Symbol "sqr", Number 10])) env `shouldBe` (env, Just (Number 100))

    it "eval recursive function calls /fact" $ do
      eval (FExpr (List [Symbol "fact", Number 10])) env
        `shouldBe` (env, Just (Number 3628800))

      
  where env = Environment (Map.fromList
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
