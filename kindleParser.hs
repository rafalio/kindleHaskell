import Control.Monad
import Text.Parsec
import Data.List
import System.Environment
import Data.Maybe

data Book = Book {
  bTitle :: String,
  bAuthor :: String
} deriving (Eq, Show)

data Highlight = Highlight  {
  bBook :: Book,
  bPage :: Maybe Int,
  bLoc :: String,
  bTime :: String,
  bContent :: String
} deriving (Eq, Show)

{-
 Possible options for the highlight location:

 Highlight Loc. 1714-16
 Highlight on Page 961

 Title line := anychars '(' author ')' endOfLine
 author := anychars


- Your Highlight on page 125 | Location 1907-1908 | Added on Tuesday
- Your Highlight on Location 941-943 | Added on Tuesday, March 17, 2015 6:22:21 AM

-}

data RTree a = Leaf a | Node [RTree a] deriving (Eq, Show)

parseParenExpr :: HighlightParser (RTree String)
parseParenExpr = try tree <|> leaf
  where
    tree = do
      char '('
      ls <- many1 parseParenExpr
      char ')'
      return $ Node ls
    --leaf = many1 (noneOf "()") >>= return . Leaf
    leaf = many1 (anyChar) >>= return . Leaf

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  f <- readFile filename
  parseTest parseHighlights f

nat :: HighlightParser Int
nat = many1 digit >>= return . read

type HighlightParser = Parsec String ()

parseHighlights :: HighlightParser [Highlight]
parseHighlights = many1 unitParser
  where
    unitParser = skipMany zeroWidthSpace >> parseHighlight >>= \h -> skipMany (char '=') >> (endOfLine >> return ()) >> return h

zeroWidthSpace = char '\65279'

-- manyTill that returns the match for the end parser also
manyTill2 :: Parsec s u a -> Parsec s u b -> Parsec s u ([a],b)
manyTill2 p end = scan
  where
    scan = (end >>= \x -> return $ ([], x)) <|> do
      x <- p
      (xs, b) <- scan
      return $ (x : xs, b)

parseBook :: HighlightParser Book
parseBook = do
  (title, author) <- manyTill2 anyChar (try authorParser)
  return $ Book title author
  where
    authorParser = between (char '(') (char ')') (many $ noneOf "()") >>= \a -> (eof <|> (endOfLine >> return ())) >> return a

parseHighlight :: HighlightParser Highlight
parseHighlight = do
  book <- parseBook
  (i,loc,time) <- parseDataLine
  skipMany (space <|> newline)
  content <- manyTill anyChar (eof <|> (endOfLine >> return ()))
  return $ Highlight book i loc time content


lexeme :: Parsec String u a -> Parsec String u a
lexeme p = p <* spaces

-- Looks like some of them don't have a Page, only a location.
parseHighlightPage :: HighlightParser Int
parseHighlightPage = (string "Page" <|> string "page") >> space >> nat

parseHighlightPagePreamble :: HighlightParser ()
parseHighlightPagePreamble =
  optional (string "Your ") >>
  (string "Highlight" <|> string "Bookmark") >>
  spaces >> string "on" >> return ()

parseLoc :: HighlightParser String
parseLoc = (try (string "Loc.") <|> string "Location")  >> spaces >> many1 (digit <|> char '-')

parseAddedTime :: HighlightParser String
parseAddedTime = do
  string "Added on" >> spaces >> manyTill anyChar ( (endOfLine >> return ()) <|> eof)

parseDataLine :: HighlightParser (Maybe Int,String,String)
parseDataLine = do
  lexeme $ char '-'
  lexeme $ parseHighlightPagePreamble
  page <- optionMaybe $ try (lexeme parseHighlightPage)
  case (isJust page) of
    True ->
      do
        lexeme (char '|')
        loc <- lexeme parseLoc
        lexeme (char '|')
        time <- lexeme parseAddedTime
        return (page,loc,time)
    False ->
      do
        loc <- lexeme parseLoc
        lexeme (char '|')
        time <- lexeme parseAddedTime
        return (page,loc,time)

sampleStr = "Technological Slavery (Theodore J. Kaczynski)"
sampleHighlightPage = "Highlight on Page 29"
sampleDataLine = "- Highlight on Page 1089 | Loc. 16686-90  | Added on Wednesday, 4 January 12 15:05:17 Greenwich Mean Time"
