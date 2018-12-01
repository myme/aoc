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
  input <- readFile "./src/Day1/Part1.input"
  return $ mapMaybe parseShift $ lines input

part1 :: IO ()
part1 = do
  ls <- sum <$> changeList
  print ls

part2 :: IO ()
part2 = do
  freqs <- scanl (+) 0 . cycle <$> changeList
  print $ go empty freqs
  where go set (f:fs) | f `member` set = f
                      | otherwise = go (f `insert` set) fs
