module Day1.Part1 where

import Data.Maybe
import Text.Read

parseShift :: String -> Maybe Int
parseShift ('+' : num) = readMaybe num
parseShift ('-' : num) = negate <$> readMaybe num
parseShift _           = Nothing

part1 :: IO ()
part1 = do
  ls <- sum . mapMaybe parseShift . lines <$> readFile "./src/Day1/Part1.input"
  print ls
