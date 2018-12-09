module Utils where

import Control.Arrow ((***), (&&&))
import Control.Monad (when)
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Data.Maybe (listToMaybe)
import Data.Semigroup (Min(..), Max(..))
import Text.ParserCombinators.ReadP

(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(...) = (.) . (.)
infixr 8 ...

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

parse :: ReadP a -> String -> Maybe a
parse = (fmap fst . listToMaybe) ... readP_to_S

readLines :: FilePath -> IO [String]
readLines filename = lines <$> readFile filename

manhattanDistance :: Num a => (a, a) -> (a, a) -> a
manhattanDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

maxBy :: Ord b => (a -> b) -> [a] -> a
maxBy = maximumBy . (compare `on`)

minBy :: Ord b => (a -> b) -> [a] -> a
minBy = minimumBy . (compare `on`)

minMax :: (Foldable t, Bounded a, Ord a) => t a -> (a, a)
minMax = (getMin *** getMax) . foldMap (Min &&& Max)

expect :: (Eq a, Show a) => String -> a -> a -> IO ()
expect msg expected actual = do
  when (expected /= actual) $ fail (show actual <> " != " <> show expected)
  putStrLn $ msg <> show actual
