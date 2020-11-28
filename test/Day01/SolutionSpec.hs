module Day01.SolutionSpec (spec) where

import Day01.Solution
import Test.Hspec

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "does" $ do
      part1 "hello" `shouldBe` "hello"
