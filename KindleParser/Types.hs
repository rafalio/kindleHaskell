module KindleParser.Types where

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Function

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

