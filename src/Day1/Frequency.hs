module Day1.Part1 where

import Data.IntSet
import Data.Maybe
import Text.Read

parseShift :: String -> Maybe Int
parseShift ('+' : num) = readMaybe num
parseShift ('-' : num) = negate <$> readMaybe num
parseShift _           = Nothing

changeList :: IO [Int]
changeList = do
  input <- readFile "./src/Day1/input.txt"
  return $ mapMaybe parseShift $ lines input

part1 :: IO Int
part1 = sum <$> changeList

part2 :: IO Int
part2 = do
  freqs <- scanl (+) 0 . cycle <$> changeList
  return $ go empty freqs
  where go set (f:fs) | f `member` set = f
                      | otherwise = go (f `insert` set) fs

puzzle :: IO ()
puzzle = do
  putStr "Answer part 1: "
  print =<< part1
  putStr "Answer part 2: "
  print =<< part2
