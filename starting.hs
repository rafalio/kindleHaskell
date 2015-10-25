{-# LANGUAGE GADTs #-}

import Text.Parsec
import qualified Data.Map.Lazy as Map
import Control.Applicative hiding ((<|>), many)


{-
e := term '+' e
e := term '-' e
e := t

term := int
term := var
term := '(' e ')'

-}

data Expr =    ExprInt Int
             | BinExpr Expr BinOp Expr
             | Var String
             deriving (Eq, Show)

data BinOp = Add | Sub deriving (Eq, Show)

type SimpleParser a = Parsec String () a
type Ident = String

type ExprEnv = Map.Map Ident Int

lexeme :: Parsec String u a -> Parsec String u a
lexeme p = p >>= \x -> (skipMany space) >> return x

parseExprInt :: SimpleParser Expr
parseExprInt = many1 digit >>= \n -> (return $ ExprInt (read n))

parseExprVar :: SimpleParser Expr
parseExprVar = many1 letter >>= \x -> return $ Var x

term :: SimpleParser Expr
term = lexeme (parseExprInt <|> parseExprVar <|> (bracketParser parseNumExpr))

bracketParser p = do
  lexeme $ char '('
  x <- lexeme p
  lexeme $ char ')'
  return x

parseBinOp :: SimpleParser BinOp
parseBinOp = lexeme (char '+' >> return Add) <|> (char '-' >> return Sub)

-- Instead of left-recursion, use iteration
parseNumExpr = do
  t <- term
  rest <- many (parseBinOp >>= \op -> term >>= \t -> return (op,t))
  return $ foldl (\x (f, y) -> BinExpr x f y) t rest

evalExpr :: Expr -> ExprEnv -> Either String Int
evalExpr (ExprInt i) env = Right i
evalExpr (BinExpr l Add r) env = liftA2 (+) (evalExpr l env) (evalExpr r env)
evalExpr (BinExpr l Sub r) env = liftA2 (-) (evalExpr l env) (evalExpr r env)
evalExpr (Var v) env = case Map.lookup v env of
  Nothing -> Left $ "Unknown variable: " ++ v
  Just val -> Right val

sampleEnv :: ExprEnv
sampleEnv = Map.fromList [("x",20)]

main :: IO ()
main = undefined

