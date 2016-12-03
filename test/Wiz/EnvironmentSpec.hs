module Wiz.EnvironmentSpec where

import qualified Wiz.EvalApply as W
import Wiz.Types
import Wiz.Environment
import Wiz.Utils
import Wiz.Parser
import Test.Hspec
import Text.Parsec (parse)

import qualified Data.Map as Map

spec = describe "Environment tests" $ do

  describe "Basic lookup" $ do

    let env = Environment (Map.fromList [("a", Number 10)])
    it "Basic variable lookup" $ do
      (envLookup "a" env) `shouldBe` (Number 10)
