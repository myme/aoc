module Aoc2015.Day3.Day3 where

import           Control.Arrow
import qualified Data.Set as S
import           Utils

type Position = (Int, Int)
type Visited = S.Set Position

input :: IO String
input = readFile "./src/Aoc2015/Day3/input.txt"

move :: Char -> Position -> Position
move = \case
  '^' -> second pred
  'v' -> second succ
  '>' -> first  pred
  '<' -> first  succ
  _   -> id

deliver :: String -> Visited
deliver = S.fromList . scanl (flip move) (0, 0)

part1 :: IO Int
part1 = S.size . deliver <$> input

part2 :: IO Int
part2 = do
  let deliver' = deliver . everyOther 1
  (santa, robo) <- (deliver' &&& deliver' . drop 1) <$> input
  pure $ S.size (santa <> robo)
