module Day1.Day1 where

import Data.IntSet
import Data.Maybe
import Text.Read

parseShift :: String -> Maybe Int
parseShift ('+' : num) = readMaybe num
parseShift num         = readMaybe num

changeList :: IO [Int]
changeList = do
  input <- readFile "./src/Day1/input.txt"
  return $ mapMaybe parseShift $ lines input

part2 :: [Int] -> Int
part2 = go empty . scanl (+) 0 . cycle
  where go set (f:fs) | f `member` set = f
                      | otherwise = go (f `insert` set) fs

puzzle :: IO ()
puzzle = do
  list <- changeList
  putStrLn $ "part 1: " <> show (sum list)
  putStrLn $ "part 2: " <> show (part2 list)
