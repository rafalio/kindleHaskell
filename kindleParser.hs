import Control.Monad
import Text.Parsec
import Data.List

data Book = Book {
  bTitle :: String,
  bAuthor :: String
} deriving (Eq, Show)

data Highlight = Highlight  {
  bBook :: Book,
  bPage :: Int,
  bLoc :: String,
  bTime :: String,
  bContent :: String
} deriving (Eq, Show)

main :: IO ()
main = do
  f <- readFile "/Users/rafal/Desktop/kindleTagOne.txt"
  print f
  parseTest parseHighlight f

nat :: HighlightParser Int
nat = many1 digit >>= return . read

type HighlightParser = Parsec String ()


zeroWidthSpace = char '\65279'

parseBook :: HighlightParser Book
parseBook = do
  title <- bookTitleParser
  author <- authorParser
  return $ Book (concat . intersperse " " $ title) author
  where
    bookTitleParser = skipMany zeroWidthSpace >> sepBy1 (many alphaNum) space
    authorParser = between (char '(') (char ')') (many (letter <|> space <|> oneOf ".'"))

parseHighlight :: HighlightParser Highlight
parseHighlight = do
  book <- parseBook >>= \b -> endOfLine >> return b
  (i,loc,time) <- parseDataLine
  skipMany (space <|> newline)
  content <- manyTill anyChar (eof <|> (endOfLine >> return ()))
  return $ Highlight book i loc time content


lexeme :: Parsec String u a -> Parsec String u a
lexeme p = p >>= \x -> spaces >> return x

parseHighlightPage :: HighlightParser Int
parseHighlightPage = string "Highlight on Page" >> space >> nat

parseLoc :: HighlightParser String
parseLoc = string "Loc." >> spaces >> many1 (digit <|> char '-')

parseAddedTime :: HighlightParser String
parseAddedTime = do
  string "Added on" >> spaces >> manyTill anyChar ( (endOfLine >> return ()) <|> eof)

parseDataLine :: HighlightParser (Int,String,String)
parseDataLine = do
  lexeme $ char '-'
  page <- lexeme parseHighlightPage
  lexeme (char '|')
  loc <- lexeme parseLoc
  lexeme (char '|')
  time <- lexeme parseAddedTime
  return (page,loc,time)


sampleStr = "Technological Slavery (Theodore J. Kaczynski)"

sampleHighlightPage = "Highlight on Page 29"

sampleDataLine = "- Highlight on Page 1089 | Loc. 16686-90  | Added on Wednesday, 4 January 12 15:05:17 Greenwich Mean Time"
