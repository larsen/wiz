module Wiz.EvalApplySpec where

import qualified Data.Map as Map
import           Test.Hspec
import           Text.Parsec (parse)
import qualified Wiz.EvalApply as W
import           Wiz.Parser
import           Wiz.Types
import           Wiz.Environment

import Data.Maybe (fromMaybe)
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
      prg <- loadProgram "init.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "1"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Number 1))

    it "eval simple arithmetic operations" $ do
      prg <- loadProgram "init.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "(+ 2 2)"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Number 4))

    it "eval simple comparison operator" $ do
      prg <- loadProgram "init.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "(< 1 2)"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Boolean True))

    it "eval simple comparison operator /2" $ do
      prg <- loadProgram "init.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "(> 1 2)"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Boolean False))

    it "eval simple comparison operator /3" $ do
      prg <- loadProgram "init.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "(> 3 2 5)"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Boolean False))

    it "eval simple comparison operator /3" $ do
      prg <- loadProgram "init.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "(>= 3 3 5)"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Boolean False))

    it "eval function calls /square" $ do
      prg <- loadProgram "test/square.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "(square 10)"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Number 100))

    it "eval recursive function calls /fact" $ do
      prg <- loadProgram "init.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "(fact 10)"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Number 3628800))

    it "eval recursive function calls /map" $ do
      prg <- loadProgram "init.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "(map fact '(1 2 3))"
      results <- W.eval expr env
      (snd results) `shouldBe` Just (E $ List [Number 1, Number 2, Number 6])

    it "eval another recursive function calls /length" $ do
      prg <- loadProgram "init.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "(length '(1 2 3))"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Number 3))

    it "eval (let) form" $ do
      prg <- loadProgram "init.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "(let ((a 10) (b 20)) a)"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Number 10))

    it "eval (let) form /2" $ do
      prg <- loadProgram "init.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "(let ((a 10) (b 20)) (+ a b))"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Number 30))

    it "eval closure form" $ do
      prg <- loadProgram "init.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "(add1 10)"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Number 11))

    it "procedure returning closure" $ do
      prg <- loadProgram "test/closure.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "(m 10)"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Number 20))

    it "count change example" $ do
      prg <- loadProgram "init.scm"
      prg' <- loadProgram "test/count-change.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      env' <- W.runProgram env $ fromMaybe (Program []) prg'
      expr <- parseForm "(count-change 100)"
      results <- W.eval expr env'
      results `shouldBe` (env', Just (E $ Number 292))

  describe "set! instructions" $ do
    it "eval set!" $ do
      prg <- loadProgram "test/set.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "a"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ Number 20))

    it "eval set-car!" $ do
      prg <- loadProgram "test/set-car.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "list"
      results <- W.eval expr env
      results `shouldBe` (env, Just (E $ List [Number 2, Number 2, Number 3]))

    it "eval set-cdr!" $ do
      prg <- loadProgram "test/set-cdr.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "list"
      results <- W.eval expr env
      (snd results) `shouldBe` Just (E $ List [Number 1, Number 20, Number 30])

    it "eval set-cdr! (it preserves list length as calculated by length)" $ do
      prg <- loadProgram "init.scm"
      prg' <- loadProgram "test/set-cdr.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      env' <- W.runProgram env $ fromMaybe (Program []) prg'
      expr <- parseForm "(length list)"
      results <- W.eval expr env'
      (snd results) `shouldBe` Just (E $ Number 3)

    it "eval set-cdr!" $ do
      prg <- loadProgram "test/set-cdr-2.scm"
      env <- W.runProgram emptyEnv $ fromMaybe (Program []) prg
      expr <- parseForm "list"
      results <- W.eval expr env
      (snd results) `shouldBe` Just (E $ List [Number 1, Number 20])
