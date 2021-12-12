module Aoc2015.Day1.Day1 where

import Data.List

stepFloor :: Int -> Char -> Int
stepFloor f '(' = f + 1
stepFloor f ')' = f - 1
stepFloor f _   = f

input :: IO String
input = readFile "./src/Aoc2015/Day1/input.txt"

part1 :: IO Int
part1 = foldl stepFloor 0 <$> input

part2 :: IO Int
part2 = maybe 0 id . findIndex (== -1) . scanl stepFloor 0 <$> input
