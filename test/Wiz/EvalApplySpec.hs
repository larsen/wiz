module Wiz.EvalApplySpec where

import qualified Data.Map as Map
import           Test.Hspec
import           Text.Parsec (parse)
import qualified Wiz.EvalApply as W
import           Wiz.Parser
import           Wiz.Types
import           Wiz.Environment

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

    it "parses form /5" $
      parse pForm "(test)" "(<= 1 2)" `shouldBe`
        Right (FExpr (List
                      [Operator "<=",
                       Number 1,
                       Number 2]))

  describe "eval" $ do

    it "eval numeric expressions as integers" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "1"
      W.eval expr env `shouldBe` (env, Just (E $ Number 1))

    it "eval simple arithmetic operations" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(+ 2 2)"
      W.eval expr env `shouldBe` (env, Just (E $ Number 4))

    it "eval simple comparison operator" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(< 1 2)"
      W.eval expr env `shouldBe` (env, Just (E $ Boolean True))

    it "eval simple comparison operator /2" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(> 1 2)"
      W.eval expr env `shouldBe` (env, Just (E $ Boolean False))

    it "eval simple comparison operator /3" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(> 3 2 5)"
      W.eval expr env `shouldBe` (env, Just (E $ Boolean False))

    it "eval simple comparison operator /3" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(>= 3 3 5)"
      W.eval expr env `shouldBe` (env, Just (E $ Boolean False))

    it "eval function calls /square" $ do
      env <- loadProgram "test/square.scm"
      expr <- parseForm "(square 10)"
      W.eval expr env `shouldBe` (env, Just (E $ Number 100))

    it "eval recursive function calls /fact" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(fact 10)"
      W.eval expr env `shouldBe` (env, Just (E $ Number 3628800))

    it "eval recursive function calls /map" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(map fact '(1 2 3))"
      (snd $ W.eval expr env) `shouldBe` Just (E $ List [Number 1, Number 2, Number 6])

    it "eval another recursive function calls /length" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(length '(1 2 3))"
      W.eval expr env `shouldBe` (env, Just (E $ Number 3))

    it "eval (let) form" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(let ((a 10) (b 20)) a)"
      W.eval expr env `shouldBe` (env, Just (E $ Number 10))

    it "eval (let) form /2" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(let ((a 10) (b 20)) (+ a b))"
      W.eval expr env `shouldBe` (env, Just (E $ Number 30))

    it "eval closure form" $ do
      env <- loadProgram "init.scm"
      expr <- parseForm "(add1 10)"
      W.eval expr env `shouldBe` (env, Just (E $ Number 11))

    it "procedure returning closure" $ do
      env <- loadProgram "test/closure.scm"
      expr <- parseForm "(m 10)"
      W.eval expr env `shouldBe` (env, Just (E $ Number 20))

  describe "set! instructions" $ do
    it "eval set!" $ do
      env <- loadProgram "test/set.scm"
      expr <- parseForm "a"
      W.eval expr env `shouldBe` (env, Just (E $ Number 20))

    it "eval set-car!" $ do
      env <- loadProgram "test/set-car.scm"
      expr <- parseForm "list"
      W.eval expr env `shouldBe` (env, Just (E $ List [Number 2, Number 2, Number 3]))

    it "eval set-cdr!" $ do
      env <- loadProgram "test/set-cdr.scm"
      expr <- parseForm "list"
      (snd $ W.eval expr env) `shouldBe` Just (E $ List [Number 1, Number 20, Number 30])

    it "eval set-cdr! (it preserves list length as calculated by length)" $ do
      env <- loadProgram "test/set-cdr.scm"
      expr <- parseForm "(length list)"
      (snd $ W.eval expr env) `shouldBe` Just (E $ Number 3)

    it "eval set-cdr!" $ do
      env <- loadProgram "test/set-cdr-2.scm"
      expr <- parseForm "list"
      (snd $ W.eval expr env) `shouldBe` Just (E $ List [Number 1, Number 20])
