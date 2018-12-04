module Main where

import qualified Day1.Day1 as Day1
import qualified Day2.Day2 as Day2
import qualified Day3.Day3 as Day3
import qualified Day4.Day4 as Day4

main :: IO ()
main = mapM_ printPuzzle $ zip [1 ..]
  [ Day1.puzzle
  , Day2.puzzle
  , Day3.puzzle
  , Day4.puzzle
  ]
  where
    printPuzzle x = do
      putStrLn $ show (fst x) <> "."
      snd x
