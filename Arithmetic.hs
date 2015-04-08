import System.Console.Haskeline
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, char, digit, string, oneOf, try, (<|>))
import Text.Parsec.Combinator (choice, many1)
import Control.Applicative (many)
import Control.Monad (void)

data Expression = Number Int
                | Operator Char [Expression]
                | Expression
                  deriving (Show, Read)

type Value = Int

-- *Main> let expr = Operator '+' [Number 1, Number 2, Number 3]
-- *Main> expr
-- Operator '+' [Number 1,Number 2,Number 3]
-- *Main> eval expr
-- 6
-- *Main> 

-- Eval

eval :: Expression -> Value
eval e =
  case e of
    Number n -> n
    Operator '+' exps -> foldl (+) 0 (map eval exps)
    Operator '*' exps -> foldl (*) 1 (map eval exps)

main :: IO ()
main = runInputT defaultSettings loop
   where 
       loop :: InputT IO ()
       loop = do
           input <- getInputLine "λ> "
           case input of
               Nothing -> return ()
               Just input -> do
                 let res = parse pExpression "(source)" input
                 case res of
                   Left err -> do
                     outputStrLn "Error!"
                     loop
                   Right form -> do
                     outputStrLn $ show $ eval form
                     loop

-- Parser

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

pNumber :: Parser Expression
pNumber = do
  void $ whitespace
  n <- many1 $ digit
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
  try (pNumber)
  <|> try (pOperator)

-- pList :: Parser Expression
-- pList = do 
--     void $ char '('
--     ts <- many $ pExpression
--     void $ char ')'
--     return ts

test rule text = parse rule "(source)" text
