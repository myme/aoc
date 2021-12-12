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

input :: IO [String]
input = readLines "./src/Aoc2015/Day5/input.txt"

isNicePart1 :: String -> Bool
isNicePart1 s = minThreeVowels s && doubleLetter s && noBadCombos s

part1 :: IO Int
part1 = length . filter isNicePart1 <$> input

letterPair :: String -> Bool
letterPair []       = False
letterPair [_]      = False
letterPair (x:y:xs) = isInfixOf [x, y] xs || letterPair (y:xs)

repeatedWithDivider :: String -> Bool
repeatedWithDivider [] = False
repeatedWithDivider (x:xs) = case xs of
  []      -> False
  [_]     -> False
  (_:y:_) -> x == y || repeatedWithDivider xs

isNicePart2 :: String -> Bool
isNicePart2 s = letterPair s && repeatedWithDivider s

part2 :: IO Int
part2 = length . filter isNicePart2 <$> input
