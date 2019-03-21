module Test.Utils where

import Test.Hspec
import Test.QuickCheck
import Utils

tests :: Spec
tests = do
  describe "everyOther" $ do
    it "with 0 is identity" $ do
      property $ \ls -> everyOther 0 ls `shouldBe` (ls :: [Int])

    it "with 1 length is either 1/2 or (1/2) + 1" $ do
      property $ \ls -> do
        let lx = length (ls :: [Int])
            ly = length (everyOther 1 ls)
        if even lx
          then ly `shouldBe` (lx `div` 2)
          else ly `shouldBe` 1 + (lx `div` 2)
