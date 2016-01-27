import Control.Monad
import Text.Parsec
import Data.List
import Data.Maybe
import Data.Char
import Data.Function

import Text.Pandoc.Definition
import Text.Pandoc.Writers.Markdown
import Text.Pandoc.Options

import Data.Time.Format
import Data.Time.Clock
import Data.Time.LocalTime

import Data.Default
import qualified Data.Map.Lazy as M
import qualified Data.Set as Set

import System.Environment
import System.Exit

-- Types

data Book = Book {
  bTitle :: String,
  bAuthor :: String
} deriving (Eq, Show)

data Highlight = Highlight  {
  bBook :: Book,
  bPage :: Maybe Int,
  bLoc :: String,
  bTime :: UTCTime,
  bContent :: String
} deriving (Eq, Show)

instance Ord Highlight where
  compare = compare `on` bTime

data BookHighlights = BookHighlights {
  highlightsBook :: Book,
  highlights :: [Highlight]
} deriving (Eq, Show)

type HighlightParser = Parsec String ()

{-
 Possible options for the highlight location:

 Highlight Loc. 1714-16
 Highlight on Page 961

 Title line := anychars '(' author ')' endOfLine
 author := anychars

- Your Highlight on page 125 | Location 1907-1908 | Added on Tuesday
- Your Highlight on Location 941-943 | Added on Tuesday, March 17, 2015 6:22:21 AM

Two possible date formats I've seen:

7 April 12 12:59:50 Greenwich Mean Time
Saturday, July 18, 2015 3:44:29 AM
-}

main :: IO ()
main = do
  args <- getArgs
  if (null args) then (printHelp >> exitFailure) else return ()
  let filename = head args
  f <- readFile filename
  let e = parse parseHighlights "" f
  either (\a -> print $ "Parsing failed: " ++ (show a))
         (\v -> writeFile "2015-11-01-kindle-book-highlights.markdown"
                          (writeMarkdown writerOpts (highlightsToPandoc $ groupHighlights v) )
         )
         e
  where
    writerOpts = def { writerStandalone = True,
                       writerTemplate = "$titleblock$\n\n\n$body$"
                     }
    printHelp = putStrLn "Usage: runhaskell kindleParser.hs clippingsFile.txt"

parseHighlightFile :: FilePath -> IO (Either ParseError [Highlight])
parseHighlightFile filename = do
  f <- readFile filename
  return $ parse parseHighlights "" f

parseHighlights :: HighlightParser [Highlight]
parseHighlights = endBy parseHighlight sepLine

sepLine = skipMany (char '=') >> endOfLine

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
  skipSpaces
  (title, author) <- manyTill2 anyChar (try $ authorParser <* endOfLine)
  return $ Book (trim title) author
  where
    authorParser = between 
                       (char '(') (char ')') 
                       (many $ noneOf "()")

parseHighlight :: HighlightParser Highlight
parseHighlight = do
  book <- parseBook
  (i,loc,time) <- parseDataLine
  content <- manyTill anyChar (endOfLine <|> lookAhead sepLine)
  return $ Highlight book i loc time content

parseHighlightPage :: HighlightParser Int
parseHighlightPage = (lexeme $ caseInsensitiveString "Page") *> nat

parseHighlightPagePreamble :: HighlightParser ()
parseHighlightPagePreamble = do
  lexeme $ optional (string "Your")
  lexeme $ choice [string "Highlight", string "Bookmark"]
  optional (string "on")

parseLoc :: HighlightParser String
parseLoc = do
  lexeme $ (try (string "Loc.") <|> string "Location")
  many1 (digit <|> char '-')

parseAddedTime :: HighlightParser UTCTime
parseAddedTime = do
  lexeme $ string "Added on"
  chars <- manyTill anyChar (lookAhead endOfLine)
  choice $ fmap (\x -> timeParse x chars) timeFormats
    where
      timeFormats = [ -- Because why not, Kindle format picks whatever it wants.
          "%A, %e %B %y %H:%M:%S Greenwich Mean Time",
          "%A, %B %e, %Y %H:%M:%S %p Greenwich Mean Time",
          "%e %B %y %H:%M:%S",
          "%A, %B %e, %Y %l:%M:%S %p"
          ]
      timeParse = parseTimeM True defaultTimeLocale

parseDataLine :: HighlightParser (Maybe Int, String, UTCTime)
parseDataLine = do
  lexeme $ char '-'
  lexeme $ parseHighlightPagePreamble
  page <- optionMaybe $ lexeme $ try parseHighlightPage
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

-- Helpers

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

-- Pandoc Generators

groupHighlights :: [Highlight] -> [BookHighlights]
groupHighlights hs = sorted
  where
    sorted = sortOn (\group -> minimum (highlights group) ) (map mkGroup grouped)
    grouped = groupBy ( (==) `on` bBook) (sortOn (bAuthor . bBook) hs)
    mkGroup bs = BookHighlights (bBook $ head bs) bs

bookHighlightToBlock :: BookHighlights -> [Block]
bookHighlightToBlock h = [header, quotes]
  where
    headerString = (bTitle book) ++ " (by " ++ bAuthor book ++ ")"
    header = Header 1 nullAttr [Str $ headerString]
    quotes = BulletList $ map quoteForHighlight (highlights h)
    quoteForHighlight highlight = [Para [Str $ bContent highlight]]
    book = highlightsBook h

highlightsToPandoc :: [BookHighlights] -> Pandoc
highlightsToPandoc hs = Pandoc meta (concatMap bookHighlightToBlock hs)
  where
    meta = Meta $ M.fromList [("title", MetaString $ "My Kindle Book Highlights"),
                              ("date", MetaString $ "October 31 2015"),
                              ("toc", MetaBool $ True)
                              ]

