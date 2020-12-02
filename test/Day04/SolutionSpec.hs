module Day04.SolutionSpec (spec) where

import Day04.Solution (part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  xit "solves Part 1" $ do
    input <- readFile "./test/Day04/input.txt"
    part1 input `shouldBe` "hello santa"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day04/input.txt"
    part2 input `shouldBe` "hello santa"
