module Main where

import qualified Day1.Day1 as Day1
import qualified Day2.Day2 as Day2
import qualified Day3.Day3 as Day3
import qualified Day4.Day4 as Day4
import qualified Day5.Day5 as Day5
import qualified Day6.Day6 as Day6
import qualified Day7.Day7 as Day7
import qualified Day8.Day8 as Day8
import qualified Day9.Day9 as Day9

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
  ]
  where
    printPuzzle x = do
      putStrLn $ show (fst x) <> "."
      snd x
