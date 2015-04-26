module Wiz.Parser (
    pForm
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

pNumber :: Parser Expression
pNumber = do
  void $ whitespace
  n <- many1 $ digit
  void $ whitespace
  return $ Number (read n)

pOperator :: Parser Expression
pOperator = do
  o <- choice [char '+', char '*', char '-']
  return $ Operator o

-- Il problema e` nelle seguenti due definizioni
pExpression :: Parser Expression
pExpression =
  try (pNumber)
  <|> try (pOperator)
  <|> try (pSymbol)
  <|> try (pQuote)
  <|> try (pIf)
  <|> try (pLambda)
  <|> try (pList)

pExpr :: Parser Form
pExpr = do
  expr <- pExpression
  return $ FExpr expr

pFormals :: Parser Formals
pFormals = do
  void $ whitespace
  void $ char '('
  formals <- many pIdentifier
  void $ char ')'
  void $ whitespace
  return $ (Formals formals)
  where formals = []

pIf :: Parser Expression
pIf = do
  void $ whitespace
  void $ char '('
  void $ whitespace
  void $ string "if"
  void $ whitespace
  test <- pExpression
  void $ whitespace
  consequent <- pExpression
  void $ whitespace
  alternate <- pExpression
  void $ char ')'
  return $ (If test consequent alternate)

pList :: Parser Expression
pList = do
  void $ whitespace
  void $ char '('
  void $ whitespace
  exprs <- many1 pExpression
  void $ char ')'
  return $ (List exprs)

pLambda :: Parser Expression
pLambda = do
  void $ whitespace
  void $ char '('
  void $ whitespace
  void $ string "lambda"
  void $ whitespace
  formals <- pFormals
  expr <- pExpression
  void $ char ')'
  return $ (Lambda formals expr)

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

pDefinition :: Parser Form
pDefinition = do
  void $ whitespace
  void $ char '('
  void $ whitespace
  void $ string "define"
  void $ whitespace
  name <- pIdentifier
  void $ whitespace
  expr <- pExpression
  void $ char ')'
  return $ FDef (Definition name expr)
  
pForm :: Parser Form
pForm =
  try (pDefinition) 
  <|> try (pExpr)

test rule text = parse rule "(source)" text
