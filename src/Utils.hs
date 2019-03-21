module Utils where

import Control.Arrow ((***), (&&&))
import Control.Monad (join, when)
import Data.Char (isDigit)
import Data.Function (on)
import Data.Ix (range)
import Data.List (maximumBy, minimumBy, sortBy)
import Data.Maybe (listToMaybe)
import Data.Semigroup (Min(..), Max(..))
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP

both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)

(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(...) = (.) . (.)
infixr 8 ...

parseInt :: ReadP Int
parseInt = read ... mappend <$> option "" (string "-") <*> many1 (satisfy isDigit)

parse :: ReadP a -> String -> Maybe a
parse = (fmap fst . listToMaybe) ... readP_to_S

readLines :: FilePath -> IO [String]
readLines filename = lines <$> readFile filename

type Point = (Int, Int)
type Bounds = (Point, Point)
getBounds :: [Point] -> (Point, Point)
getBounds points = let
  (minX, maxX) = minMax $ map fst points
  (minY, maxY) = minMax $ map snd points
  in ((minX, minY), (maxX, maxY))

getCoords :: Bounds -> [Point]
getCoords = sortBy (compare `on` swap) . range

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

sublistIndex :: Eq a => [a] -> [a] -> Int
sublistIndex _ [] = -1
sublistIndex as xxs@(_:xs)
  | all (uncurry (==)) $ zip as xxs = 0
  | otherwise = 1 + sublistIndex as xs

everyOther :: Int -> [a] -> [a]
everyOther n (x:xs) = x : everyOther n (drop n xs)
everyOther _ x = x
