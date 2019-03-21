module Aoc2015.Day3.Day3 where

import qualified Data.Set as S

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

deliver :: Position -> String -> Visited -> Visited
deliver pos []     visited = S.singleton pos <> visited
deliver pos (v:vs) visited = S.singleton pos <> deliver (move v pos) vs visited

part1 :: IO Int
part1 = do
  moves <- input
  let visited = deliver (0, 0) moves S.empty
  pure (S.size visited)

splitMoves :: [a] -> ([a], [a])
splitMoves = go False ([], [])
  where go _ (xs, ys) [] = (reverse xs, reverse ys)
        go l (xs, ys) (n:ns)
          | l         = go (not l) (n:xs, ys) ns
          | otherwise = go (not l) (xs, n:ys) ns

part2 :: IO Int
part2 = do
  (santa, robo) <- splitMoves <$> input
  let deliver' moves = deliver (0, 0) moves S.empty
      visited = deliver' santa <> deliver' robo
  pure (S.size visited)
