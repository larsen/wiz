module Wiz.EnvironmentSpec where

import qualified Wiz.EvalApply as W
import Data.Map
import Wiz.Types
import Wiz.Environment
import Wiz.Parser
import Test.Hspec
import Text.Parsec (parse)
import Control.Exception (evaluate)

import qualified Data.Map as Map

spec = describe "Environment tests" $ do

  describe "Basic lookup" $ do

    let env = extendEnvironment emptyEnv $ fromList [("a", Value (Number 10))]
    it "Basic variable lookup" $ do
      (envLookup "a" env) `shouldBe` (Value (Number 10))
    it "Unbound symbol" $ do
      evaluate (envLookup "b" env) `shouldThrow` errorCall "Unbound symbol \"b\""

  describe "Enclosed frames" $ do
    let env = encloseEnvironment (extendEnvironment emptyEnv $ Data.Map.fromList [("a", Value (Number 10))]) -- parent
              (extendEnvironment emptyEnv $ Data.Map.fromList [("b", Value (Number 20))]) -- child
    it "Lookup in parent environment /1" $ do
      (envLookup "a" env) `shouldBe` (Value (Number 10))
    it "Lookup in parent environment /2" $ do
      (envLookup "b" env) `shouldBe` (Value (Number 20))
