module Utils where

import Control.Monad (when)
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Text.ParserCombinators.ReadP

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

readLines :: FilePath -> IO [String]
readLines filename = lines <$> readFile filename

maxBy :: Ord b => (a -> b) -> [a] -> a
maxBy = maximumBy . (compare `on`)

minBy :: Ord b => (a -> b) -> [a] -> a
minBy = minimumBy . (compare `on`)

expect :: (Eq a, Show a) => String -> a -> a -> IO ()
expect msg expected actual = do
  when (expected /= actual) $ fail (show actual <> " != " <> show expected)
  putStrLn $ msg <> show actual
