{-# LANGUAGE GADTs #-}

import Control.Monad
import Text.Parsec

data NumExpr =   ExprInt Int
               | ExprAdd NumExpr NumExpr
               | ExprSub NumExpr NumExpr
               deriving (Eq, Show)




type SimpleParser a = Parsec String () a


parseExprInt :: SimpleParser NumExpr
parseExprInt = many1 digit >>= \n -> (return $ ExprInt (read n))

parseExprAdd = do
  l <- parseNumExpr
  char '+'
  r <- parseNumExpr
  return $ ExprAdd l r

parseExprSub = do
  l <- parseNumExpr
  char '-'
  r <- parseNumExpr
  return $ ExprSub l r

parseNumExpr :: SimpleParser NumExpr
parseNumExpr = do
  parseExprInt <|> parseExprAdd <|> parseExprSub

sampleExpr = ExprSub (ExprInt 12) (ExprAdd (ExprInt 2) (ExprInt 3))

evalExpr :: NumExpr -> Int
evalExpr (ExprInt i)    = i
evalExpr (ExprAdd l r)  = (evalExpr l) + (evalExpr r)
evalExpr (ExprSub l r)  = (evalExpr l) - (evalExpr r)
--evalExpr (ExprMul l r)  = (evalExpr l) * (evalExpr r)

main :: IO ()
main = undefined



