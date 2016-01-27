module KindleParser.Parser (
  parseHighlights,
  parseHighlightString,
) where

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

import Text.Parsec
import Data.Maybe

import Data.Time.Format
import Data.Time.Clock
import Data.Time.LocalTime

import KindleParser.Types
import KindleParser.ParserUtils

type HighlightParser = Parsec String ()

parseHighlightString :: String -> Either ParseError [Highlight]
parseHighlightString = parse parseHighlights ""

parseHighlights :: HighlightParser [Highlight]
parseHighlights = endBy parseHighlight sepLine

sepLine = skipMany (char '=') >> endOfLine

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


