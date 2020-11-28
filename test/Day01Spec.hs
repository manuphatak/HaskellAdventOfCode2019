module Day01Spec (spec) where

import Day01.Day01
import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "Part 1" $ do
    it "does" $ do
      part1 "hello" `shouldBe` "hello"