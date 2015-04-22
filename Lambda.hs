import System.Console.Haskeline
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, char, digit, string, oneOf, noneOf, try, (<|>))
import Text.Parsec.Combinator (choice, many1, manyTill)
import Control.Applicative (many)
import Control.Monad (void)
import qualified Data.Map as Map
import qualified Data.List as L
import Text.Printf
import Debug.Trace


data Formals = Formals [String]
  deriving (Show)

data Expression = Number Integer
                | Operator Char [Expression]
                | Symbol String
                | If Expression Expression Expression
                | Lambda Formals Expression
                | LambdaApply String [Expression]
                | Expression

instance Show Expression where
  show (Number n) = show n
  show (Symbol s) = show s
  show (Operator c exprs) =
    "(" ++ show c ++ " " ++ L.unwords (map show exprs) ++ ")"
  show (If test consequent alternate) =
    "(if " ++ show test ++ show consequent ++ show alternate ++ ")"
  show (Lambda (Formals formals) expr) =
    "(λ ( " ++ (L.unwords (map show formals)) ++ show expr ++ ")"
  show (LambdaApply s exprs) = "( " ++ show s ++ " " ++ (L.unwords (map show exprs)) ++ " )"

data Definition = Definition String Expression
  deriving (Show)

data Form = FDef Definition | FExpr Expression
  deriving (Show)

type Value = Integer

data Environment = Environment (Map.Map String Expression)

instance Show Environment where
  show (Environment env) =
    L.unlines (map showPair (Map.toList env)) ++ "\n"
    where showPair (k, v) = show k ++ "\t->\t" ++ show v

-- This is slightly (!) wrong
data Arguments = Arguments [Expression]

envLookup :: String -> Environment -> Expression
envLookup symbol env
  | trace ("envlookup " ++ show symbol ++ " in " ++ show env) False = undefined
envLookup symbol (Environment env) =
  case res of
    Just res -> res
    Nothing -> undefined
  where res = Map.lookup symbol env

eval :: Form -> Environment -> (Environment, Maybe Value)
eval form env =
  case form of
    FDef def -> (evalDefinition def env, Nothing)
    FExpr expr -> (env, Just $ evalExpr env expr)

evalDefinition :: Definition -> Environment -> Environment
evalDefinition (Definition symbol expr) (Environment env) =
  Environment (Map.insert symbol expr env)

-- FIXME ugly
evalInBooleanContext :: Integer -> Bool
evalInBooleanContext n | trace ("evalInBooleanContext " ++ show n) False = undefined
evalInBooleanContext 0 = False
evalInBooleanContext _ = True

evalExpr :: Environment -> Expression -> Value
-- evalExpr env expr
--   | trace ("evalExpr " ++ show expr ++ " in " ++ show env) False = undefined

evalExpr env e =
  case e of
    Number n -> n
    Symbol s -> evalExpr env (envLookup s env)
    Operator '+' exps -> foldl (+) 0 (map (evalExpr env) exps)
    Operator '*' exps -> foldl (*) 1 (map (evalExpr env) exps)
    Operator '-' (expr:exps) -> foldl (-)
                               (evalExpr env expr)
                               (map (evalExpr env) exps)
    If test consequent alternate ->
      if (evalInBooleanContext $ evalExpr env test) then
        (evalExpr env consequent)
      else (evalExpr env alternate)
    Lambda formals expression -> 0 -- not sure about this return value
    LambdaApply symbol exprs -> apply env (envLookup symbol env) exprs

-- Apply

-- "A procedure is, slightly simplified,
-- an abstraction of an expression over objects."

apply :: Environment -> Expression -> [Expression] -> Value
apply env _ _ | trace ("apply in " ++ show env) False = undefined
apply env (Lambda (Formals formals) expr) arguments =
  evalExpr env' expr
  where env' = extendEnvironment env (zip formals arguments)
        -- FIXME non sto valutando le espressioni!
        extendEnvironment (Environment env) newElems =
          Environment (Map.union env (Map.fromList newElems))
apply _ _ _ = undefined


main :: IO ()
main = runInputT defaultSettings (loop env)
   where
       env = Environment (Map.fromList
                          [("fact",Lambda (Formals ["n"])
                                   (If (Symbol "n")
                                    (Operator '*'
                                     [Symbol "n",
                                      LambdaApply "fact" [Operator '-' [Symbol "n",Number 1]]])
                                    (Number 1)))])
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
                       Just result -> outputStrLn $ printf "%d\n%s\n" result (show env')
                       Nothing     -> outputStrLn $ printf "%s\n" (show env')
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
  o <- choice [char '+', char '*', char '-']
  exprs <- many $ pExpression
  void $ char ')'
  return $ Operator o exprs

-- Il problema e` nelle seguenti due definizioni
pExpression :: Parser Expression
pExpression =
  try (pNumber)
  <|> try (pOperator)
  <|> try (pSymbol)
  <|> try (pIf)
  <|> try (pLambda)
  <|> try (pLambdaApply)

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

pLambdaApply :: Parser Expression
pLambdaApply = do
  void $ whitespace
  void $ char '('
  symbol <- pIdentifier
  void $ whitespace
  exprs <- many1 pExpression
  void $ whitespace
  void $ char ')'
  return $ LambdaApply symbol exprs


pIdentifier :: Parser String
pIdentifier = do
  void $ whitespace
  name <- many1 (noneOf " ()\n\t")
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
