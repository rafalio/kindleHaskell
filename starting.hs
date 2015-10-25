{-# LANGUAGE GADTs #-}

import Control.Monad
import Text.Parsec


{-
e := term '+' e
e := term '-' e
e := t

term := int
term := var
term := '(' e ')'

-}

data Expr      = ExprInt Int
               | BinExpr Expr BinOp Expr
               | Var String
               deriving (Eq, Show)

data BinOp = Add | Sub deriving (Eq, Show)

type SimpleParser a = Parsec String () a

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

evalExpr :: Expr -> Int
evalExpr (ExprInt i)    = i
evalExpr (BinExpr l Add r)  = (evalExpr l) + (evalExpr r)
evalExpr (BinExpr l Sub r)  = (evalExpr l) - (evalExpr r)

main :: IO ()
main = undefined

