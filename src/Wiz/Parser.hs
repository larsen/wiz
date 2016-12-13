module Wiz.Parser (
    pForm,
    pProgram,
    loadProgram
  ) where

import Wiz.Types
import Wiz.Environment
import Wiz.EvalApply
import Text.Parsec.Char (spaces)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (parens)
import Text.Parsec (ParseError, parse, char, digit, string, oneOf, noneOf, try, (<|>))
import Text.Parsec.Combinator (choice, many1, manyTill)
import Control.Applicative (many)
import Control.Monad (void)

-- Parser

openParens = do
  void spaces
  void $ char '('
  void spaces

closedParens = do
  void spaces
  void $ char ')'
  void spaces

betweenParens p = do
  void openParens
  r <- try p
  void closedParens
  return r

pBoolean :: Parser Expression
pBoolean = do
  void spaces
  try (do
    string "#t"
    return (Boolean True))
  <|>
  try (do
    string "#f"
    return (Boolean False))

pNumber :: Parser Expression
pNumber = do
  void spaces
  n <- many1 digit
  void spaces
  return $ Number (read n)

pOperator :: Parser Expression
pOperator = do
  o <- choice [ try (string "+")
              , try (string "*")
              , try (string "-")
              , try (string ">=")
              , try (string "<=")
              , try (string ">")
              , try (string "<") ]
  void spaces
  return $ Operator o

-- Il problema e` nelle seguenti due definizioni
pExpression :: Parser Expression
pExpression =
  try pNumber
  <|> try pBoolean
  <|> try pOperator
  <|> try pSymbol
  <|> try pDefinition
  <|> try pQuote
  <|> try pIf
  <|> try pLambda
  <|> try pSet
  <|> try pList

pExpr :: Parser Form
pExpr = do
  expr <- pExpression
  return $ FExpr expr

pFormals :: Parser Formals
pFormals = do
  betweenParens $ do
    formals <- many pIdentifier
    return $ Formals formals
  where formals = []

pIf :: Parser Expression
pIf = do
  betweenParens $ do
    void $ string "if"
    void spaces
    test <- pExpression
    void spaces
    consequent <- pExpression
    void spaces
    alternate <- pExpression
    return $ If test consequent alternate

pList :: Parser Expression
pList = do
  betweenParens $ do
    exprs <- many pExpression
    return $ List exprs

pSet :: Parser Expression
pSet = do
  betweenParens $ do
    void $ string "set!"
    void spaces
    name <- pIdentifier
    void spaces
    expr <- pExpression
    void spaces
    return $ SetInstruction name expr

pLambda :: Parser Expression
pLambda = do
  betweenParens $ do
    void $ string "lambda"
    void spaces
    formals <- pFormals
    expr <- pExpression
    return $ Lambda formals expr

pIdentifier :: Parser String
pIdentifier = do
  void spaces
  name <- many1 (noneOf " '()\n\t")
  void spaces
  return name

pSymbol :: Parser Expression
pSymbol = do
  void spaces
  name <- many1 (noneOf " '()\n\t")
  void spaces
  return $ Symbol name

pQuote :: Parser Expression
pQuote = do
  void spaces
  void $ char '\''
  expr <- pExpression
  return $ Quote expr

pDefinition :: Parser Expression
pDefinition = do
  betweenParens $ do
    void $ string "define"
    void spaces
    name <- pIdentifier
    void spaces
    expr <- pExpression
    return $ Definition name expr
  
pForm :: Parser Form
pForm = try pExpr

pProgram :: Parser Program
pProgram = do
  forms <- many pForm
  return $ Program forms

-- test rule = parse rule "(source)"

loadProgram :: String -> IO Environment
loadProgram file = do
  program <- readFile file
  putStrLn $ "Loading '" ++ file ++ "'"
  let res = parse pProgram "(source)" program
  case res of
    Left err -> do
      putStrLn "Error occurred parsing file."
      return emptyEnv
    Right p -> do
      putStrLn "Init file parsed correctly."
      return $ runProgram emptyEnv p
  where
    runProgram env (Program (x:xs)) =
      let (env', res) = eval x env
      in case res of
        Just res -> runProgram env' (Program xs)
        Nothing  -> runProgram env' (Program xs) -- ???
    runProgram env (Program []) = env
