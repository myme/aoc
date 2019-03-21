module Aoc2015.Day3.Day3 where

import           Control.Arrow
import qualified Data.Set as S
import           Utils

type Position = (Int, Int)
type Visited = S.Set Position

input :: IO String
input = readFile "./src/Aoc2015/Day3/input.txt"

move :: Char -> Position -> Position
move c (x, y) = case c of
  '^' -> (x, y - 1)
  'v' -> (x, y + 1)
  '>' -> (x + 1, y)
  '<' -> (x - 1, y)
  _   -> (x, y)

deliver :: String -> Visited
deliver moves = go (0, 0) moves S.empty
  where go pos []     visited = S.singleton pos <> visited
        go pos (v:vs) visited = S.singleton pos <> go (move v pos) vs visited

part1 :: IO Int
part1 = S.size . deliver <$> input

part2 :: IO Int
part2 = do
  let deliver' = deliver . everyOther 1
  (santa, robo) <- (deliver' &&& deliver' . drop 1) <$> input
  pure $ S.size (santa <> robo)
