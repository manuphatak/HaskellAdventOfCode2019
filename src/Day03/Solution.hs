module Day03.Solution where

import Data.Either
import Data.List
import qualified Data.Map.Strict as Map
import Text.Parsec

part1 :: String -> String
part1 = show . closestIntersection . lines

part2 :: String -> String
part2 = id

data Direction = U | R | D | L deriving (Read, Show)

type Distance = Int

data Instruction = Instruction Direction Distance deriving (Show)

type Wire = [Instruction]

closestIntersection :: [String] -> Distance
closestIntersection = minimum . map (manhattanDistance centralPoint) . gridIntersections . asGrid . map (wirePath . readWire)

solve :: [Int] -> Distance
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

data Coordinates = Coordinates Int Int deriving (Show, Ord, Eq)

type Grid = Map.Map Coordinates [Int]

centralPoint :: Coordinates
centralPoint = Coordinates 0 0

emptyGrid :: Grid
emptyGrid = Map.empty :: Grid

wirePath :: Wire -> [Coordinates]
wirePath [] = []
wirePath (z : zs) = go centralPoint z zs
  where
    go :: Coordinates -> Instruction -> Wire -> [Coordinates]
    go coordinates (Instruction _ 0) (y : ys) = go coordinates y ys
    go coordinates (Instruction _ 0) [] = [coordinates]
    go coordinates (Instruction direction distance) wire = coordinates : go (next direction coordinates) (Instruction direction (pred distance)) wire

    next :: Direction -> Coordinates -> Coordinates
    next U (Coordinates x y) = Coordinates x (succ y)
    next R (Coordinates x y) = Coordinates (succ x) y
    next D (Coordinates x y) = Coordinates x (pred y)
    next L (Coordinates x y) = Coordinates (pred x) y

addWire :: Int -> Grid -> [Coordinates] -> Grid
addWire index = foldl' fn
  where
    fn :: Grid -> Coordinates -> Grid
    fn grid coordinates = Map.insertWith (++) coordinates [index] grid

asGrid :: [[Coordinates]] -> Grid
asGrid = go 0 emptyGrid
  where
    go _ grid [] = grid
    go index grid (x : xs) = go (succ index) (addWire index grid x) xs

gridIntersections :: Grid -> [Coordinates]
gridIntersections = filter (Coordinates 0 0 /=) . Map.keys . Map.filter ((> 1) . length . nub)

manhattanDistance :: Coordinates -> Coordinates -> Int
manhattanDistance (Coordinates ax ay) (Coordinates bx by) = abs (ax - bx) + abs (ay - by)

-- >>> input = ["R8,U5,L5,D3", "U7,R6,D4,L4"]
-- >>> map (manhattanDistance centralPoint) . closestIntersection' $ input
-- [6,11]
