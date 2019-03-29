module Aoc2015.Day5.Day5 where

import Data.List
import Utils

badCombos :: [String]
badCombos = ["ab", "cd", "pq", "xy"]

minThreeVowels :: String -> Bool
minThreeVowels = (>= 3) . length . filter (`elem` vowels)
  where vowels = "aeiou" :: String

doubleLetter :: String -> Bool
doubleLetter = any ((>= 2) . length) . group

noBadCombos :: String -> Bool
noBadCombos s = not . any (\(a, b) -> [a, b] `elem` badCombos) $ zip s (drop 1 s)

isNice :: String -> Bool
isNice s = minThreeVowels s && doubleLetter s && noBadCombos s

input :: IO [String]
input = readLines "./src/Aoc2015/Day5/input.txt"

part1 :: IO Int
part1 = length . filter isNice <$> input
