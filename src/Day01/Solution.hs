module Day01.Solution where

part1 :: String -> String
part1 = show . sum . map (fuelRequiredForMass . readInt) . lines

part2 :: String -> String
part2 = show . sum . map (fuelRequiredForMassAndFuel . readInt) . lines

fuelRequiredForMass :: Int -> Int
fuelRequiredForMass mass = (mass `div` 3) - 2

fuelRequiredForMassAndFuel :: Int -> Int
fuelRequiredForMassAndFuel = go . fuelRequiredForMass
  where
    go :: Int -> Int
    go fuelRequired
      | fuelRequired >= 0 = fuelRequired + fuelRequiredForMassAndFuel fuelRequired
      | otherwise = 0

readInt :: String -> Int
readInt x = read x :: Int
