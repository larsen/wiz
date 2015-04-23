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

instance Show Expression where
  show (Number n) = show n
  show (Symbol s) = show s
  show (Operator c exprs) =
    "( " ++ show c ++ " " ++ L.unwords (map show exprs) ++ " )"
  show (If test consequent alternate) =
    "(if " ++ show test ++ " " ++ show consequent ++ " " ++ show alternate ++ ")"
  show (Lambda (Formals formals) expr) =
    "(λ ( " ++ (L.unwords (map show formals)) ++ " ) ( " ++ show expr ++ " ) )"
  show (LambdaApply s exprs) =
    "( " ++ show s ++ " " ++ (L.unwords (map show exprs)) ++ " )"

data Definition = Definition String Expression
  deriving (Show)

data Form = FDef Definition | FExpr Expression
  deriving (Show)

data Environment = Environment (Map.Map String Expression)

instance Show Environment where
  show (Environment env) =
    L.unlines (map showPair (Map.toList env)) ++ "\n"
    where showPair (k, v) = show k ++ "\t->\t" ++ show v

envLookup :: String -> Environment -> Expression
-- envLookup symbol env
--   | trace ("envlookup " ++ show symbol ++ " in\n" ++ show env) False = undefined
envLookup symbol (Environment env) =
  case res of
    Just res -> res
    Nothing -> undefined
  where res = Map.lookup symbol env

eval :: Form -> Environment -> (Environment, Maybe Expression)
eval form env =
  case form of
    FDef def -> (evalDefinition def env, Nothing)
    FExpr expr -> (env, Just $ evalExpr env expr)

evalDefinition :: Definition -> Environment -> Environment
evalDefinition (Definition symbol expr) (Environment env) =
  Environment (Map.insert symbol expr env)

-- FIXME ugly
evalInBooleanContext :: Expression -> Bool
-- evalInBooleanContext n | trace ("evalInBooleanContext " ++ show n) False = undefined
evalInBooleanContext (Number 0) = False
evalInBooleanContext _ = True

evalNumeric :: [Expression] -> [Integer]
-- evalNumeric n | trace ("evalNumeric " ++ show n) False = undefined
evalNumeric [] = []
evalNumeric ((Number x):xs) = x : evalNumeric xs

evalNumericElem :: Expression -> Integer
evalNumericElem (Number n) = n
evalNumericElem _ = undefined

evalExpr :: Environment -> Expression -> Expression
-- evalExpr env expr
--   | trace ("evalExpr " ++ show expr ++ " in\n" ++ show env) False = undefined
evalExpr env expression =
  case expression of
    Number n -> Number n
    Symbol s -> evalExpr env (envLookup s env)
    
    Operator '*' exps -> Number (foldl (*) 1 (evalNumeric (map (evalExpr env) exps)))
    Operator '+' exps -> Number (foldl (+) 0 (evalNumeric (map (evalExpr env) exps)))
    Operator '-' (exp:exps) ->
      Number (foldl (-)
              (evalNumericElem $ evalExpr env exp)
              (evalNumeric (map (evalExpr env) exps)))

    If test consequent alternate ->
      (if (evalInBooleanContext $ evalExpr env test) then
         (evalExpr env consequent)
       else (evalExpr env alternate))

    Lambda formals expression -> Number 0 -- not sure about this return value
    LambdaApply symbol exprs -> apply env (envLookup symbol env) exprs


-- Apply

-- "A procedure is, slightly simplified,
-- an abstraction of an expression over objects."

apply :: Environment -> Expression -> [Expression] -> Expression
-- apply env _ _ | trace ("apply in\n" ++ show env) False = undefined
apply env (Lambda (Formals formals) expr) arguments =
  evalExpr env' expr
  where env' = extendEnvironment env (zip formals evaledArguments)
        -- FIXME non sto valutando le espressioni!
        extendEnvironment (Environment env) newElems =
          Environment (Map.union (Map.fromList newElems) env)
        evaledArguments = map (evalExpr env) arguments
apply _ _ _ = undefined


main :: IO ()
main = runInputT defaultSettings (loop env)
   where
       env = Environment (Map.fromList
                          -- A couple of functions for tests
                          [("fact",Lambda (Formals ["n"])
                                   (If (Symbol "n")
                                    (Operator '*'
                                     [Symbol "n",
                                      LambdaApply "fact" [Operator '-' [Symbol "n",Number 1]]])
                                    (Number 1))),
                           ("sqr",Lambda (Formals ["n"])
                                  (Operator '*' [Symbol "n", Symbol "n"]))])
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
                       -- Just result -> outputStrLn $ printf "%s\n%s\n" (show result) (show env')
                       Just result -> outputStrLn $ printf "%s\n" (show result)
                       Nothing     -> outputStrLn $ printf "\n" -- (show env')
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
