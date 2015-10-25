{-# LANGUAGE GADTs #-}

import Control.Monad
import Text.Parsec


{-
1 + 2 - 4

need to left-factor

e := int
e := e '+' e
e := e '-' 'e'


e := term '+' e
e := term '-' e
e := t

term := int

-}

data NumExpr =   ExprInt Int
--               | ExprAdd NumExpr NumExpr
 --              | ExprSub NumExpr NumExpr
               | BinExpr NumExpr BinOp NumExpr
               deriving (Eq, Show)

data BinOp = Add | Sub deriving (Eq, Show)

type SimpleParser a = Parsec String () a

parseExprInt :: SimpleParser NumExpr
parseExprInt = many1 digit >>= \n -> (return $ ExprInt (read n))

lexeme :: Parsec String u a -> Parsec String u a
lexeme p = p >>= \x -> (skipMany space) >> return x

term :: SimpleParser NumExpr
term = lexeme parseExprInt

parseBinOp :: SimpleParser BinOp
parseBinOp = lexeme (char '+' >> return Add) <|> (char '-' >> return Sub)

-- Instead of left-recursion, use iteration
parseNumExpr = do
  t <- term
  rest <- many (parseBinOp >>= \op -> term >>= \t -> return (op,t))
  return $ foldl (\x (f, y) -> BinExpr x f y) t rest

--sampleExpr = ExprSub (ExprInt 12) (ExprAdd (ExprInt 2) (ExprInt 3))

evalExpr :: NumExpr -> Int
evalExpr (ExprInt i)    = i
evalExpr (BinExpr l Add r)  = (evalExpr l) + (evalExpr r)
evalExpr (BinExpr l Sub r)  = (evalExpr l) - (evalExpr r)

main :: IO ()
main = undefined



