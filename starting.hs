{-# LANGUAGE GADTs #-}

import Text.Parsec
import qualified Data.Map.Lazy as Map
import Control.Applicative hiding ((<|>), many)
import Control.Monad.State

{-
r := var <- e
r := e

e := term '+' e
e := term '-' e
e := t

term := int
term := var
term := '(' e ')'

-}

data CalcExpr = Assign Var Expr | EffectExpr Expr
  deriving (Show, Eq)

data Expr =    ExprInt Int
             | BinExpr Expr BinOp Expr
             | ExprVar Var
             deriving (Eq, Show)

type Ident = String
data Var = Var Ident deriving (Eq,Show)

data BinOp = Add | Sub deriving (Eq, Show)

type SimpleParser a = Parsec String () a

type ExprEnv = Map.Map Ident Int

lexeme :: Parsec String u a -> Parsec String u a
lexeme p = p >>= \x -> (skipMany space) >> return x

parseAssign :: SimpleParser CalcExpr
parseAssign = do
  (ExprVar v) <- parseExprVar
  lexeme $ string "<-"
  e <- parseNumExpr
  return $ Assign v e

parseTopLevel = try parseAssign <|> parseEffectExpr
  where
    parseEffectExpr = parseNumExpr >>= \x -> return (EffectExpr x)

parseExprInt :: SimpleParser Expr
parseExprInt = lexeme $ many1 digit >>= \n -> (return $ ExprInt (read n))

parseExprVar :: SimpleParser Expr
parseExprVar = lexeme $ many1 letter >>= \x -> return $ ExprVar (Var x)

term :: SimpleParser Expr
term = lexeme (parseExprInt <|> parseExprVar <|> (bracketParser parseNumExpr))

bracketParser p = do
  lexeme $ char '('
  x <- lexeme p
  lexeme $ char ')'
  return x

parseBinOp :: SimpleParser BinOp
parseBinOp = lexeme $ (char '+' >> return Add) <|> (char '-' >> return Sub)

-- Instead of left-recursion, use iteration
parseNumExpr = do
  t <- term
  rest <- many (parseBinOp >>= \op -> term >>= \t -> return (op,t))
  return $ foldl (\x (f, y) -> BinExpr x f y) t rest

evalExpr :: Expr -> ExprEnv -> Either String Int
evalExpr (ExprInt i) env = Right i
evalExpr (BinExpr l Add r) env = liftA2 (+) (evalExpr l env) (evalExpr r env)
evalExpr (BinExpr l Sub r) env = liftA2 (-) (evalExpr l env) (evalExpr r env)
evalExpr (ExprVar (Var v)) env = case Map.lookup v env of
  Nothing -> Left $ "Unknown variable: " ++ v
  Just val -> Right val

evalCalcExpr :: CalcExpr -> ExprEnv -> Either String (Int, ExprEnv)
evalCalcExpr (Assign (Var v) e) env = do
  val <- evalExpr e env
  return $ (val, Map.insert v val env)
evalCalcExpr (EffectExpr e) env = do
  ret <- evalExpr e env
  return $ (ret, env)

sampleEnv :: ExprEnv
sampleEnv = Map.fromList [("x",20)]

main :: IO ()
main = calcLoop mempty

calcLoop :: ExprEnv -> IO ()
calcLoop env = do
  inp <- getLine
  case (runCalc inp env) of
    Left err -> putStrLn ("Error: " ++ err) >> calcLoop env
    Right (i,env') -> putStrLn ("Result: " ++ show i) >> calcLoop env'

runCalc :: String -> ExprEnv -> Either String (Int, ExprEnv)
runCalc inp env = case parse parseTopLevel "" inp of
  Left perror -> Left (show perror)
  Right expr -> evalCalcExpr expr env
