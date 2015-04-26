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
  where env = (Environment (Map.fromList []))
