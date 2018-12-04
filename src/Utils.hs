module Utils where

import           Data.Char (isDigit)
import           Text.ParserCombinators.ReadP

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

readLines :: FilePath -> IO [String]
readLines filename = lines <$> readFile filename
