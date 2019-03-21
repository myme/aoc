module Aoc2015.Day3.Day3 where

import qualified Data.Set as S

type Position = (Int, Int)
type Visited = S.Set Position

input :: IO String
input = readFile "./src/Aoc2015/Day3/input.txt"

move :: Position -> String -> Visited -> Visited
move pos [] visited = S.singleton pos <> visited
move (x, y) (v:vs) visited =
  let pos = case v of
              '^' -> (x, y - 1)
              'v' -> (x, y + 1)
              '>' -> (x + 1, y)
              '<' -> (x - 1, y)
              _   -> (x, y)
  in S.singleton (x, y) <> move pos vs visited

part1 :: IO Int
part1 = do
  moves <- input
  let visited = move (0, 0) moves S.empty
  pure (S.size visited)
