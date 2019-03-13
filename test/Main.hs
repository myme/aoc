module Main where

import qualified Aoc2018.Day1.Day1 as Day1
import qualified Aoc2018.Day2.Day2 as Day2
import qualified Aoc2018.Day3.Day3 as Day3
import qualified Aoc2018.Day4.Day4 as Day4
import qualified Aoc2018.Day5.Day5 as Day5
import qualified Aoc2018.Day6.Day6 as Day6
import qualified Aoc2018.Day7.Day7 as Day7
import qualified Aoc2018.Day8.Day8 as Day8
import qualified Aoc2018.Day9.Day9 as Day9
import qualified Aoc2018.Day10.Day10 as Day10
import qualified Aoc2018.Day11.Day11 as Day11
import qualified Aoc2018.Day12.Day12 as Day12
import qualified Aoc2018.Day13.Day13 as Day13
import qualified Aoc2018.Day14.Day14 as Day14
import qualified Aoc2018.Day15.Day15 as Day15

main :: IO ()
main = mapM_ printPuzzle $ zip ([1 ..] :: [Integer])
  [ Day1.puzzle
  , Day2.puzzle
  , Day3.puzzle
  , Day4.puzzle
  , Day5.puzzle
  , Day6.puzzle
  , Day7.puzzle
  , Day8.puzzle
  , Day9.puzzle
  , Day10.puzzle
  , Day11.puzzle
  , Day12.puzzle
  , Day13.puzzle
  , Day14.puzzle
  , Day15.puzzle
  ]
  where
    printPuzzle x = do
      putStrLn $ show (fst x) <> "."
      snd x
