module KindleParser.MarkdownGen (
  highlightListToPandoc,
) where

import Text.Pandoc.Definition
import Text.Pandoc.Writers.Markdown
import Text.Pandoc.Options

import Data.List
import qualified Data.Map.Strict as M

import Data.Function

import KindleParser.Types

highlightListToPandoc :: [Highlight] -> Pandoc
highlightListToPandoc = highlightsToPandoc . groupHighlights

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
