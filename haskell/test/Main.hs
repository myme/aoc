module Main where

import qualified Test.Aoc2015 as Aoc2015
import qualified Test.Aoc2018 as Aoc2018
import           Test.Hspec
import qualified Test.Utils as Utils

main :: IO ()
main = hspec $ do
  describe "Advent of Code 2015" Aoc2015.tests
  describe "Advent of Code 2018" Aoc2018.tests
  describe "Utils" Utils.tests
