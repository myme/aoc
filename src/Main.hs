module Main where

import qualified Day1.Day1 as Day1
import qualified Day2.Day2 as Day2
import qualified Day3.Day3 as Day3
import qualified Day4.Day4 as Day4

main :: IO ()
main = do
  putStrLn "Day 1"
  Day1.puzzle

  putStrLn "Day 2"
  Day2.puzzle

  putStrLn "Day 3"
  Day3.puzzle

  putStrLn "Day 4"
  Day4.puzzle
