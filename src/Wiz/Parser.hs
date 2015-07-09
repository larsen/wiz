module Wiz.Parser (
    pForm,
    pProgram
  ) where

import Wiz.Types
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (parens)
import Text.Parsec (parse, char, digit, string, oneOf, noneOf, try, (<|>))
import Text.Parsec.Combinator (choice, many1, manyTill)
import Control.Applicative (many)
import Control.Monad (void)

-- Parser

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

openParens = do
  void whitespace
  void $ char '('
  void whitespace

closedParens = do
  void whitespace
  void $ char ')'
  void whitespace

pBoolean :: Parser Expression
pBoolean = do
  void whitespace
  try (do
    string "#t"
    return (Boolean True))
  <|>
  try (do
    string "#f"
    return (Boolean False))

pNumber :: Parser Expression
pNumber = do
  void whitespace
  n <- many1 digit
  void whitespace
  return $ Number (read n)

pOperator :: Parser Expression
pOperator = do
  o <- choice [char '+', char '*', char '-']
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
  <|> try pList

pExpr :: Parser Form
pExpr = do
  expr <- pExpression
  return $ FExpr expr

pFormals :: Parser Formals
pFormals = do
  openParens
  formals <- many pIdentifier
  closedParens
  return $ Formals formals
  where formals = []

pIf :: Parser Expression
pIf = do
  openParens
  void $ string "if"
  void whitespace
  test <- pExpression
  void whitespace
  consequent <- pExpression
  void whitespace
  alternate <- pExpression
  closedParens
  return $ If test consequent alternate

pList :: Parser Expression
pList = do
  openParens
  exprs <- many pExpression
  closedParens
  return $ List exprs

pLambda :: Parser Expression
pLambda = do
  openParens
  void $ string "lambda"
  void whitespace
  formals <- pFormals
  expr <- pExpression
  closedParens
  return $ Lambda WU.emptyEnv formals expr

pIdentifier :: Parser String
pIdentifier = do
  void $ whitespace
  name <- many1 (noneOf " '()\n\t")
  void $ whitespace
  return name

pSymbol :: Parser Expression
pSymbol = do
  void $ whitespace
  name <- many1 (noneOf " '-+()\n\t")
  void $ whitespace
  return $ Symbol name

pQuote :: Parser Expression
pQuote = do
  void $ whitespace
  void $ char '\''
  expr <- pExpression
  return $ (Quote expr)

pDefinition :: Parser Expression
pDefinition = do
  openParens
  void $ string "define"
  void $ whitespace
  name <- pIdentifier
  void $ whitespace
  expr <- pExpression
  closedParens
  return $ Definition name expr
  
pForm :: Parser Form
pForm = try (pExpr)

pProgram :: Parser Program
pProgram = do
  forms <- many pForm
  return $ Program forms

test rule = parse rule "(source)"

loadProgram :: String -> IO Environment
loadProgram file = do
  program <- readFile file
  putStrLn $ "Loading '" ++ file ++ "'"
  let res = parse pProgram "(source)" program
  case res of
    Left err -> do
      putStrLn "Error occurred parsing file."
      return WU.emptyEnv
    Right p -> do
      putStrLn "Init file parsed correctly."
      return $ runProgram WU.emptyEnv p
  where
    runProgram env (Program (x:xs)) =
      let (env', res) = eval x env
      in case res of
        Just res -> runProgram env' (Program xs)
        Nothing  -> runProgram env' (Program xs) -- ???
    runProgram env (Program []) = env
