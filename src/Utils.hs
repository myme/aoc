module Utils where

import Data.Char (isDigit)
import Data.Function (on)
import Data.List (maximumBy)
import Text.ParserCombinators.ReadP

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

readLines :: FilePath -> IO [String]
readLines filename = lines <$> readFile filename

maxBy :: Ord b => (a -> b) -> [a] -> a
maxBy = maximumBy . (compare `on`)
