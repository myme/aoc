module Aoc2018.Day5.Day5 where

import           Data.Char (isAlpha, isLower, toLower)
import qualified Data.Set as S
import           Utils

react :: String -> String
react = go ""
  where a >< b = a /= b && toLower a == toLower b
        go prev "" = reverse prev
        go "" (n:next) = go [n] next
        go (p:prev) (n:next) | p >< n = go prev next
                             | otherwise = go (n:p:prev) next

puzzle :: IO ()
puzzle = do
  input <- filter isAlpha <$> readFile "./src/Aoc2018/Day5/input.txt"

  let part1 = length $ react input
  expect "part 1: " 9238 part1

  let
    lower = S.toList $ S.fromList $ filter isLower input
    removed = map (\x -> react $ filter ((/= x) . toLower) input) lower
    part2 = minimum $ map length removed
  expect "part 2: " 4052 part2
