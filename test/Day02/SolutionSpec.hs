module Day02.SolutionSpec (spec) where

import Day02.Solution
import Test.Hspec

spec :: Spec
spec = do
  xit "solves Part 1" $ do
    input <- readFile "./test/Day02/input.txt"
    part1 input `shouldBe` "hello santa"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day02/input.txt"
    part2 input `shouldBe` "hello santa"
