module Wiz.Parser (
    pForm,
    pProgram,
    loadProgram
  ) where

import Wiz.Types
-- import Wiz.Environment
import Text.Parsec.Char (spaces)
import Text.Parsec.String (Parser)
import Text.Parsec (anyChar, endOfLine, parse, char, digit, string, noneOf, try, sepBy, (<|>))
import Text.Parsec.Combinator (choice, many1, manyTill)
import Control.Applicative (many)
import Control.Monad (void)

-- Parser

separator :: Parser ()
separator = do
  void $ try comment <|> try spaces
  return ()

openParens :: Parser ()
openParens = do
  void separator
  void $ char '('
  void separator

closedParens :: Parser ()
closedParens = do
  void separator
  void $ char ')'
  void separator

betweenParens p = do
  void openParens
  r <- try p
  void closedParens
  return r

comment :: Parser ()
comment = do
  void $ char ';'
  void $ manyTill anyChar endOfLine

pBoolean :: Parser Expression
pBoolean = do
  void separator
  try (do
    string "#t"
    return (Boolean True))
  <|>
  try (do
    string "#f"
    return (Boolean False))

pNumber :: Parser Expression
pNumber = do
  void separator
  n <- many1 digit
  void separator
  return $ Number (read n)

pOperator :: Parser Expression
pOperator = do
  o <- choice [ try (string "+")
              , try (string "*")
              , try (string "/")
              , try (string "-")
              , try (string ">=")
              , try (string "<=")
              , try (string ">")
              , try (string "<") ]
  void separator
  return $ Operator o

-- Il problema e` nelle seguenti due definizioni
pExpression :: Parser Expression
pExpression =
  try pNumber
  <|> try pString
  <|> try pBoolean
  <|> try pOperator
  <|> try pSymbol
  <|> try pDefinition
  <|> try pQuote
  <|> try pCond
  <|> try pLambda
  <|> try pSet
  <|> try pSetCar
  <|> try pSetCdr
  <|> try pList

pExpr :: Parser Form
pExpr = do
  expr <- pExpression
  return $ FExpr expr

pFormals :: Parser Formals
pFormals = betweenParens $ do
  formals <- many pIdentifier
  return $ Formals formals

pClause :: Parser Expression
pClause = betweenParens $ do
  test <- pExpression
  void separator
  consequent <- pExpression
  return $ Clause test consequent

pCond :: Parser Expression
pCond = betweenParens $ do
  void $ string "cond"
  void separator
  clauses <- many pClause
  return $ Cond clauses

pList :: Parser Expression
pList = betweenParens $ do
    exprs <- many pExpression
    return $ List exprs

pIdentifierAndExpression :: Parser (String, Expression)
pIdentifierAndExpression = do
  void separator
  name <- pIdentifier
  void separator
  expr <- pExpression
  void separator
  return (name, expr)

pSet :: Parser Expression
pSet = betweenParens $ do
    void $ string "set!"
    (name, expr) <- pIdentifierAndExpression
    return $ SetInstruction name expr

pSetCar :: Parser Expression
pSetCar = betweenParens $ do
    void $ string "set-car!"
    (name, expr) <- pIdentifierAndExpression
    return $ SetCarInstruction name expr

pSetCdr :: Parser Expression
pSetCdr = betweenParens $ do
    void $ string "set-cdr!"
    (name, expr) <- pIdentifierAndExpression
    return $ SetCdrInstruction name expr

pLambda :: Parser Expression
pLambda = betweenParens $ do
    void $ string "lambda"
    void separator
    formals <- pFormals
    expr <- pExpression
    return $ Lambda formals expr

pString :: Parser Expression
pString = do
  void $ char '"'
  string <- many (noneOf "\"")
  void $ char '"'
  return $ String string

pIdentifier :: Parser String
pIdentifier = do
  void separator
  name <- many1 (noneOf " '()\n\t")
  void separator
  return name

pSymbol :: Parser Expression
pSymbol = do
  void separator
  name <- many1 (noneOf " '()\n\t")
  void separator
  return $ Symbol name

pQuote :: Parser Expression
pQuote = do
  void separator
  void $ char '\''
  expr <- pExpression
  return $ Quote expr

pDefinition :: Parser Expression
pDefinition = betweenParens $ do
  void $ string "define"
  (name, expr) <- pIdentifierAndExpression
  return $ Definition name expr
  
pForm :: Parser Form
pForm = try pExpr

pProgram :: Parser Program
pProgram = do
  void $ separator
  forms <- sepBy pForm separator
  return $ Program forms

-- test rule = parse rule "(source)"

loadProgram :: String -> IO (Maybe Program)
loadProgram file = do
  putStrLn $ "Loading '" ++ file ++ "'"
  program <- readFile file
  let res = parse pProgram "(source)" program
  case res of
    Left err -> do
      putStrLn "Error occurred parsing file."
      return Nothing
    Right p -> do
      putStrLn (file ++ " parsed correctly.")
      return $ Just p
