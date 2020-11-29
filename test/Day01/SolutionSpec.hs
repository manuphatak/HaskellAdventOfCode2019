{-# LANGUAGE RecordWildCards #-}

module Day01.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day01.Solution
import Test.Hspec

spec :: Spec
spec = do
  it "solves Part 1" $ do
    input <- readFile "./test/Day01/input.txt"
    part1 input `shouldBe` "3497399"
  it "solves Part 2" $ do
    input <- readFile "./test/Day01/input.txt"
    part2 input `shouldBe` "5243207"

  describe "fuelRequiredForMass" fuelRequiredForMassSpec
  describe "fuelRequiredForMassAndFuel" fuelRequiredForMassAndFuelSpec

data FuelRequiredCase = FuelRequiredCase
  {mass :: Int, expected :: Int}

fuelRequiredForMassSpec :: Spec
fuelRequiredForMassSpec =
  let cases :: [FuelRequiredCase]
      cases =
        [ FuelRequiredCase {mass = 12, expected = 2},
          FuelRequiredCase {mass = 14, expected = 2},
          FuelRequiredCase {mass = 1969, expected = 654},
          FuelRequiredCase {mass = 100756, expected = 33583}
        ]

      test :: FuelRequiredCase -> Spec
      test FuelRequiredCase {..} =
        it ("is " ++ show expected ++ " given a mass of " ++ show mass) $
          fuelRequiredForMass mass `shouldBe` expected
   in for_ cases test

fuelRequiredForMassAndFuelSpec :: Spec
fuelRequiredForMassAndFuelSpec =
  let cases :: [FuelRequiredCase]
      cases =
        [ FuelRequiredCase {mass = 12, expected = 2},
          FuelRequiredCase {mass = 14, expected = 2},
          FuelRequiredCase {mass = 1969, expected = 966},
          FuelRequiredCase {mass = 100756, expected = 50346}
        ]

      test :: FuelRequiredCase -> Spec
      test FuelRequiredCase {..} =
        it ("is " ++ show expected ++ " given a mass of " ++ show mass) $
          fuelRequiredForMassAndFuel mass `shouldBe` expected
   in for_ cases test
