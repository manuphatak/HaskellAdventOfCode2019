module Day03.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day03.Solution
import Test.Hspec

spec :: Spec
spec = do
  xit "solves Part 1" $ do
    input <- readFile "./test/Day03/input.txt"
    part1 input `shouldBe` "hello santa"

  xit "solves Part 2" $ do
    input <- readFile "./test/Day03/input.txt"
    part2 input `shouldBe` "hello santa"

  describe "closestIntersection" $ do
    for_
      [ (["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"], 159),
        (["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"], 135)
      ]
      test
  where
    test :: ([String], Int) -> Spec
    test (input, expected) =
      it "solves for the nearest intersection" $
        closestIntersection input `shouldBe` expected
