{-# LANGUAGE RecordWildCards #-}

module Day02.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day02.Solution
  ( part1,
    part2,
    readProgram,
    run,
    showProgram,
  )
import Test.Hspec

spec :: Spec
spec = do
  it "solves Part 1" $ do
    input <- readFile "./test/Day02/input.txt"
    part1 input `shouldBe` "5305097"
  it "solves Part 2" $ do
    input <- readFile "./test/Day02/input.txt"
    part2 input `shouldBe` "4925"

  describe "run" runSpec

data RunSpecCase = RunSpecCase
  {input :: String, output :: String}

runSpec :: Spec
runSpec =
  let cases :: [RunSpecCase]
      cases =
        [ RunSpecCase {input = "1,0,0,0,99", output = "2,0,0,0,99"},
          RunSpecCase {input = "2,3,0,3,99", output = "2,3,0,6,99"},
          RunSpecCase {input = "2,4,4,5,99,0", output = "2,4,4,5,99,9801"},
          RunSpecCase {input = "1,1,1,4,99,5,6,0,99", output = "30,1,1,4,2,5,6,0,99"}
        ]

      test :: RunSpecCase -> Spec
      test RunSpecCase {..} =
        it ("converts to " ++ output ++ " given " ++ input) $
          (showProgram . run . readProgram) input `shouldBe` output
   in for_ cases test
