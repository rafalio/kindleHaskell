module KindleParser.KindleParser where

import Control.Monad

import System.Environment
import System.Exit

import Text.Parsec
import Data.Default

import Text.Pandoc.Definition
import Text.Pandoc.Writers.Markdown
import Text.Pandoc.Options

import KindleParser.Types
import KindleParser.Parser
import KindleParser.MarkdownGen

main :: IO ()
main = do
  args <- getArgs
  if (null args) then (printHelp >> exitFailure) else return ()
  let filename = head args
  f <- readFile filename
  let e = parse parseHighlights "" f
  either (\a -> print $ "Parsing failed: " ++ (show a))
         (\v -> writeFile "2015-11-01-kindle-book-highlights.markdown"
                          (writeMarkdown writerOpts (highlightListToPandoc v) )
         )
         e
  where
    writerOpts = def { writerStandalone = True,
                       writerTemplate = "$titleblock$\n\n\n$body$"
                     }
    printHelp = putStrLn "Usage: runhaskell kindleParser.hs clippingsFile.txt"

parseHighlightFile :: FilePath -> IO (Either ParseError [Highlight])
parseHighlightFile f = readFile f >>= return . parseHighlightString
