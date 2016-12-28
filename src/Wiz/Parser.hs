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
  where formals = []

pClause :: Parser Expression
pClause = betweenParens $ do
  test <- pExpression
  void spaces
  consequent <- pExpression
  return $ Clause test consequent

pCond :: Parser Expression
pCond = betweenParens $ do
  void $ string "cond"
  void spaces
  clauses <- many pClause
  return $ Cond clauses

pList :: Parser Expression
pList = betweenParens $ do
    exprs <- many pExpression
    return $ List exprs

pSet :: Parser Expression
pSet = betweenParens $ do
    void $ string "set!"
    void spaces
    name <- pIdentifier
    void spaces
    expr <- pExpression
    void spaces
    return $ SetInstruction name expr

pSetCar :: Parser Expression
pSetCar = betweenParens $ do
    void $ string "set-car!"
    void spaces
    name <- pIdentifier
    void spaces
    expr <- pExpression
    void spaces
    return $ SetCarInstruction name expr

pSetCdr :: Parser Expression
pSetCdr = betweenParens $ do
    void $ string "set-cdr!"
    void spaces
    name <- pIdentifier
    void spaces
    expr <- pExpression
    void spaces
    return $ SetCdrInstruction name expr

pLambda :: Parser Expression
pLambda = betweenParens $ do
    void $ string "lambda"
    void spaces
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
pDefinition = betweenParens $ do
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

loadProgram :: String -> Environment -> IO Environment
loadProgram file env = do
  program <- readFile file
  putStrLn $ "Loading '" ++ file ++ "'"
  let res = parse pProgram "(source)" program
  case res of
    Left err -> do
      putStrLn "Error occurred parsing file."
      return env
    Right p -> do
      putStrLn (file ++ " parsed correctly.")
      return $ runProgram env p
  where
    runProgram env (Program (x:xs)) =
      let (env', res) = eval x env
      in case res of
        Just res -> runProgram env' (Program xs)
        Nothing  -> runProgram env' (Program xs) -- ???
    runProgram env (Program []) = env
