module Test.Aoc2015 where

import qualified Aoc2015.Day1.Day1 as Day1
import qualified Aoc2015.Day2.Day2 as Day2
import qualified Aoc2015.Day3.Day3 as Day3

import Test.Hspec

tests :: Spec
tests = do
  describe "Day 1" $ do
    it "solves part 1" $ do
      result <- Day1.part1
      result `shouldBe` 138

    it "solves part 2" $ do
      result <- Day1.part2
      result `shouldBe` 1771

  describe "Day 2" $ do
    it "solves part 1" $ do
      result <- Day2.part1
      result `shouldBe` 1606483

    it "solves part 2" $ do
      result <- Day2.part2
      result `shouldBe` 3842356

  describe "Day 3" $ do
    it "solves part 1" $ do
      result <- Day3.part1
      result `shouldBe` 2565

    it "solves part 2" $ do
      result <- Day3.part2
      result `shouldBe` 2639

  it "solves Day 4" pending
  it "solves Day 5" pending
  it "solves Day 6" pending
  it "solves Day 7" pending
  it "solves Day 8" pending
  it "solves Day 9" pending
  it "solves Day 10" pending
  it "solves Day 11" pending
  it "solves Day 12" pending
  it "solves Day 13" pending
  it "solves Day 14" pending
  it "solves Day 15" pending
  it "solves Day 16" pending
  it "solves Day 17" pending
  it "solves Day 18" pending
  it "solves Day 19" pending
  it "solves Day 20" pending
  it "solves Day 21" pending
  it "solves Day 22" pending
  it "solves Day 23" pending
  it "solves Day 24" pending
