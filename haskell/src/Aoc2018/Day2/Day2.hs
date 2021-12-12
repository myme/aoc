module Aoc2018.Day2.Day2 where

import qualified Data.IntSet as ISet
import           Data.List (group, sort)
import           Data.Maybe (catMaybes, listToMaybe, fromMaybe)
import           Utils

doublesAndTriples :: String -> (Int, Int)
doublesAndTriples = (\x -> (has x 2, has x 3))
  . ISet.fromList -- create a set of unique lengths
  . map length    -- compute length of sublists
  . group         -- split into sub-lists of equal characters
  . sort          -- sort characters together
  where has set x = fromEnum $ x `ISet.member` set

part1 :: [String] -> Int
part1 input =
  let (doubles, triples) = unzip . map doublesAndTriples $ input
  in sum doubles * sum triples

diffStrings :: String -> String -> [Maybe Char]
diffStrings = zipWith $ \x y -> if x == y then Just x else Nothing

offByOne :: [Maybe Char] -> Bool
offByOne = (== 1) . length . filter (== Nothing)

part2 :: [String] -> Maybe String
part2 input =
  let diffs = [diffStrings s1 s2 | s1 <- input, s2 <- input]
  in listToMaybe $ map catMaybes $ filter offByOne diffs

puzzle :: IO ()
puzzle = do
  input <- readLines "./src/Aoc2018/Day2/input.txt"
  expect "part 1: " 5434 (part1 input)
  expect "part 2: " "agimdjvlhedpsyoqfzuknpjwt" $ fromMaybe "No matches!" (part2 input)
