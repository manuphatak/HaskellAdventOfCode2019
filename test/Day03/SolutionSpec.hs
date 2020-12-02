module Day03.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day03.Solution
  ( Coordinates (..),
    Direction (..),
    Instruction (..),
    closestIntersection,
    fastestIntersection,
    part1,
    part2,
    wirePath,
  )
import Test.Hspec

spec :: Spec
spec = do
  it "solves Part 1" $ do
    input <- readFile "./test/Day03/input.txt"
    part1 input `shouldBe` "731"

  it "solves Part 2" $ do
    input <- readFile "./test/Day03/input.txt"
    part2 input `shouldBe` "5672"

  describe "closestIntersection" $ do
    let test :: ([String], Int) -> Spec
        test (input, expected) =
          it "solves for the nearest intersection" $
            closestIntersection input `shouldBe` expected
        cases =
          [ (["R8,U5,L5,D3", "U7,R6,D4,L4"], 6),
            (["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"], 159),
            (["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"], 135)
          ]
     in for_ cases test

  describe "fastestIntersection" $ do
    let test :: ([String], Int) -> Spec
        test (input, expected) =
          it "solves for the nearest intersection" $
            fastestIntersection input `shouldBe` expected
        cases =
          [ (["R8,U5,L5,D3", "U7,R6,D4,L4"], 30),
            (["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"], 610),
            (["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"], 410)
          ]
     in for_ cases test

    describe "wirePath" $ do
      it "converts a wire into coordinates" $
        let wire = [Instruction R 3, Instruction U 2, Instruction L 1, Instruction D 4]
            expected =
              [ Coordinates 0 0,
                Coordinates 1 0,
                Coordinates 2 0,
                Coordinates 3 0,
                Coordinates 3 1,
                Coordinates 3 2,
                Coordinates 2 2,
                Coordinates 2 1,
                Coordinates 2 0,
                Coordinates 2 (-1),
                Coordinates 2 (-2)
              ]
         in wirePath wire
              `shouldBe` expected