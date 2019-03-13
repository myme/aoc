module Test.Aoc2015 where

import qualified Aoc2015.Day1.Day1 as Day1

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
    result `shouldBe` 0

  it "solves Day 2" pending
  it "solves Day 3" pending
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
