module Aoc2015.Day2.Day2 where

import Data.Maybe
import Text.ParserCombinators.ReadP
import Utils

input :: IO [String]
input = readLines "./src/Aoc2015/Day2/input.txt"

data Box = Box { _l :: Int, _w :: Int, _h :: Int } deriving Show

parseBox :: String -> Maybe Box
parseBox = parse $ do
  l <- parseInt <* char 'x'
  w <- parseInt <* char 'x'
  h <- parseInt <* eof
  pure (Box l w h)

paperForBox :: Box -> Int
paperForBox (Box l w h) =
  let sides = [l*w, w*h, h*l]
  in 2 * sum sides + minimum sides

ribbonForBox :: Box -> Int
ribbonForBox (Box l w h) =
  let perims = map (*2) [l+w, w+h, h+l]
  in minimum perims + l * w * h

boxes :: IO [Box]
boxes = map (fromJust . parseBox) <$> input

part1 :: IO Int
part1 = sum . map paperForBox <$> boxes

part2 :: IO Int
part2 = sum . map ribbonForBox <$> boxes
