module Wiz.EvalApplySpec where

import qualified Wiz.EvalApply as W
import Wiz.Types
import Wiz.Parser
import Test.Hspec
import Text.Parsec (parse)
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec.Error (
  ParseError,
  Message,
  errorMessages,
  messageEq )

parseForm str = do
  let res = parse pForm "(source)" str
  case res of
    Left err -> error "Something gone wrong!"
    Right f  -> return f

spec = describe "main" $ do
  
  describe "parse" $ do
    it "parses form /1 (number)" $
      parse pForm "(test)" "1" `shouldBe` Right (FExpr (Number 1))
  
    it "parses form /2" $
      parse pForm "(test)" "(1 2)" `shouldBe`
        Right (FExpr (List [Number 1, Number 2]))

    it "parses form /3" $
      parse pForm "(test)" "(let ((a 10)) a)" `shouldBe`
        Right (FExpr (List
                      [Symbol "let",
                       List [List [Symbol "a", Number 10]], Symbol "a"]))

    it "parses form /4" $
      parse pForm "(test)" "(let ((a 10) (b 20)) a)" `shouldBe`
        Right (FExpr (List
                      [Symbol "let",
                       List [List [Symbol "a", Number 10],
                             List [Symbol "b", Number 20]], Symbol "a"]))


  describe "eval" $ do

    it "eval numeric expressions as integers" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "1"
      W.eval expr env `shouldBe` (env, Just (Number 1))

    it "eval simple arithmetic operations" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(+ 2 2)"
      W.eval expr env `shouldBe` (env, Just (Number 4))

    it "eval function calls /square" $ do
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

    it "eval (let) form" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(let ((a 10) (b 20)) a)"
      W.eval expr env `shouldBe` (env, Just (Number 10))
