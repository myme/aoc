module Aoc2018.Day1.Day1 where

import Data.IntSet
import Data.Maybe
import Text.Read
import Utils

parseShift :: String -> Maybe Int
parseShift ('+' : num) = readMaybe num
parseShift num         = readMaybe num

changeList :: IO [Int]
changeList = do
  input <- readLines "./src/Aoc2018/Day1/input.txt"
  return $ mapMaybe parseShift input

part2 :: [Int] -> Int
part2 = go empty . scanl (+) 0 . cycle
  where go set (f:fs) | f `member` set = f
                      | otherwise = go (f `insert` set) fs
        go _ _ = undefined

puzzle :: IO ()
puzzle = do
  list <- changeList
  expect "part 1: " 531 (sum list)
  expect "part 2: " 76787 (part2 list)
