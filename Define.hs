import System.Console.Haskeline
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, char, anyChar, digit, string, oneOf, noneOf, try, (<|>))
import Text.Parsec.Combinator (choice, many1, manyTill)
import Control.Applicative (many)
import Control.Monad (void)
import qualified Data.Map as Map
import Text.Printf

-- *Main> let expr = Operator '+' [Number 1, Number 2, Number 3]
-- *Main> expr
-- Operator '+' [Number 1,Number 2,Number 3]
-- *Main> eval expr
-- 6
-- *Main> 

-- Eval

data Expression = Number Integer
                | Operator Char [Expression]
                | Symbol String
                | Expression
                  deriving (Show, Read)

data Definition = Definition String Expression
  deriving (Show)

data Form = FDef Definition | FExpr Expression
  deriving (Show)

type Value = Integer

data Environment = Environment (Map.Map String Value)
  deriving (Show)

eval :: Form -> Environment -> (Environment, Maybe Value)
eval form env =
  case form of
    FDef def -> (evalDefinition def env, Nothing)
    FExpr expr -> (env, Just $ evalExpr env expr)

evalDefinition :: Definition -> Environment -> Environment
evalDefinition (Definition symbol expr) (Environment env) =
  Environment (Map.insert symbol (evalExpr (Environment env) expr) env)

evalExpr :: Environment -> Expression -> Value
evalExpr (Environment env) (Symbol s) =
  case res of
    Just res -> res
    Nothing -> 0
  where res = Map.lookup s env
evalExpr env e =
  case e of
    Number n -> n
    Operator '+' exps -> foldl (+) 0 (map (evalExpr env) exps)
    Operator '*' exps -> foldl (*) 1 (map (evalExpr env) exps)

-- Potrei implementare (let ...)
-- per fare esperimenti con lo scope locale


-- Apply
-- Come rappresentare una procedura? E` una espressione
-- "A procedure is, slightly simplified, an abstraction of an expression over objects."
-- Come rappresentare gli argomenti? E` una lista di simboli che devono
--  bindati ai parametri quando si applica la procedura


-- apply :: Procedure -> Arguments -> Value
-- apply = undefined


main :: IO ()
main = runInputT defaultSettings (loop env)
   where
       env = Environment (Map.fromList [])
       loop :: Environment -> InputT IO ()
       loop env = do
           input <- getInputLine "λ> "
           case input of
               Nothing -> return ()
               Just input -> do
                 let res = parse pForm "(source)" input
                 case res of
                   Left err -> do
                     outputStrLn "Error!"
                     loop env
                   Right form -> do
                     let (env', result) = eval form env
                     case result of
                       Just result -> outputStrLn $ printf "%d\n" result
                       Nothing     -> outputStrLn $ printf ""
                     loop env'

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
  void $ whitespace
  void $ char '('
  o <- choice [char '+', char '*']
  exprs <- many $ pExpression
  void $ char ')'
  return $ Operator o exprs

pExpression :: Parser Expression
pExpression =
  try (pNumber) <|> try (pOperator) <|> try (pSymbol)

pExpr :: Parser Form
pExpr = do
  expr <- pExpression
  return $ FExpr expr

pIdentifier :: Parser String
pIdentifier = do
  void $ whitespace
  name <- many (noneOf " \n\t")
  void $ whitespace
  return name

pSymbol :: Parser Expression
pSymbol = do
  void $ whitespace
  name <- many1 (noneOf " -+()\n\t")
  void $ whitespace
  return $ Symbol name

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

-- pList :: Parser Expression
-- pList = do 
--     void $ char '('
--     ts <- many $ pExpression
--     void $ char ')'
--     return ts

test rule text = parse rule "(source)" text
