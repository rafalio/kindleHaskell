module KindleParser.ParserUtils where

import Text.Parsec
import Data.Char
import Data.Function

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

lexeme :: Parsec String u a -> Parsec String u a
lexeme p = p <* skipSpaces

-- Who knows why, sometimes there is a zero-width space separating things!
skipSpaces :: Parsec String u ()
skipSpaces = skipMany (space <|> zeroWidthSpace)
  where
    zeroWidthSpace = char '\65279'

caseInsensitiveString :: String -> Parsec String u String
caseInsensitiveString s = sequence [choice [char $ toLower c, char $ toUpper c] | c <- s]

nat :: Parsec String u Int
nat = many1 digit >>= return . read

-- manyTill that returns the match for the end parser also
manyTill2 :: Parsec s u a -> Parsec s u b -> Parsec s u ([a],b)
manyTill2 p end = scan
  where
    scan = (end >>= \x -> return $ ([], x)) <|> do
      x <- p
      (xs, b) <- scan
      return $ (x : xs, b)
