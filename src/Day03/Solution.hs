module Day03.Solution where

import Data.Either
import Text.Parsec

part1 :: String -> String
part1 = id

part2 :: String -> String
part2 = id

data Direction = U | R | D | L deriving (Read, Show)

type Distance = Int

data Instruction = Instruction Direction Distance deriving (Show)

type Wire = [Instruction]

closestIntersection :: [String] -> Distance
closestIntersection = solve . map readWire

solve :: [Wire] -> Int
solve _ = 0

readWires :: String -> [Wire]
readWires = map readWire . lines

readWire :: String -> Wire
readWire input = fromRight ([] :: Wire) $ parse parseWire "" input

parseInstruction :: Parsec String st Instruction
parseInstruction = Instruction <$> directionParser <*> distanceParser
  where
    distanceParser :: Parsec String st Distance
    distanceParser = read <$> many1 digit

    directionParser :: Parsec String st Direction
    directionParser = read <$> count 1 (oneOf ['U', 'R', 'D', 'L'])

parseWire :: Parsec String st Wire
parseWire = parseInstruction `sepBy` char ','
