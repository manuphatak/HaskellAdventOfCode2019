module Day03.Solution
  ( closestIntersection,
    fastestIntersection,
    part1,
    part2,
    wirePath,
    Coordinates (..),
    Direction (..),
    Instruction (..),
  )
where

import Data.Either
import Data.List
import qualified Data.Map.Strict as Map
import Text.Parsec

part1 :: String -> String
part1 = show . closestIntersection . lines

part2 :: String -> String
part2 = show . fastestIntersection . lines

data Direction = U | R | D | L deriving (Read, Show)

type Distance = Int

data Instruction = Instruction Direction Distance deriving (Show)

type Wire = [Instruction]

closestIntersection :: [String] -> Distance
closestIntersection = minimum . map (manhattanDistance centralPoint) . Map.keys . gridIntersections . asGrid . map (wirePath . readWire)

fastestIntersection :: [String] -> Distance
fastestIntersection = minimum . map totalLatency . Map.elems . gridIntersections . asGrid . map (wirePath . readWire)

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

data GridWire = GridWire Int Int deriving (Show)

instance Eq GridWire where
  (==) (GridWire a _) (GridWire b _) = a == b

type Grid = Map.Map Coordinates [GridWire]

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

addWire :: Int -> [Coordinates] -> Grid -> Grid
addWire index cs g = foldl' fn g (zip [0 ..] cs)
  where
    fn :: Grid -> (Int, Coordinates) -> Grid
    fn grid (steps, coordinates) = Map.insertWith (++) coordinates [GridWire index steps] grid

asGrid :: [[Coordinates]] -> Grid
asGrid coordinates = go 0 coordinates emptyGrid
  where
    go :: Int -> [[Coordinates]] -> Grid -> Grid
    go _ [] grid = grid
    go index (x : xs) grid = go (succ index) xs (addWire index x grid)

gridIntersections :: Grid -> Grid
gridIntersections = Map.filterWithKey (\k _ -> k /= centralPoint) . Map.filter ((> 1) . length . nub)

manhattanDistance :: Coordinates -> Coordinates -> Int
manhattanDistance (Coordinates ax ay) (Coordinates bx by) = abs (ax - bx) + abs (ay - by)

totalLatency :: [GridWire] -> Int
totalLatency = foldl' fn 0
  where
    fn acc (GridWire _ steps) = acc + steps
