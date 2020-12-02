module Day02.Solution (part1, part2, readProgram, run, showProgram) where

import Data.Either (fromRight)
import Data.List (find, intercalate)
import Data.Maybe (fromJust)
import Text.Parsec (char, digit, many, parse, sepBy)
import Text.Parsec.String (Parser)

type Memory = [Int]

data Program = Program Int Memory deriving (Show)

part1 :: String -> String
part1 = show . simulation (12, 2) . readProgram

part2 :: String -> String
part2 = show . prepareSolution . fromJust . goalSeek 19690720 . readProgram
  where
    prepareSolution :: (Int, Int) -> Int
    prepareSolution (noun, verb) = 100 * noun + verb

simulation :: (Int, Int) -> Program -> Int
simulation (noun, verb) = head . programMemory . run . setVerb verb . setNoun noun

line :: Parser [Int]
line = number `sepBy` char ','
  where
    number = read <$> many digit

readProgram :: String -> Program
readProgram input = go (parse line "" input)
  where
    go :: Either a Memory -> Program
    go memory = Program 0 (fromRight [] memory)

showProgram :: Program -> String
showProgram (Program _ memory) = intercalate "," (map show memory)

run :: Program -> Program
run program@(Program pointer memory) = runOpCode (memory !! pointer) program

runOpCode :: Int -> Program -> Program
runOpCode 1 = binaryOp (+)
runOpCode 2 = binaryOp (*)
runOpCode 99 = id
runOpCode opcode = error ("Unknown opcode " ++ show opcode)

binaryOp :: (Int -> Int -> Int) -> Program -> Program
binaryOp fn (Program pointer memory) =
  let addressA = (memory !! (pointer + 1))
      addressB = (memory !! (pointer + 2))
      destination = (memory !! (pointer + 3))

      valueA = (memory !! addressA)
      valueB = (memory !! addressB)

      nextPointer = pointer + 4
      nextMemory = set destination (fn valueA valueB) memory
   in run (Program nextPointer nextMemory)

set :: Int -> a -> [a] -> [a]
set position value = go 0
  where
    go _ [] = []
    go n (x : xs)
      | n == position = value : go (succ n) xs
      | otherwise = x : go (succ n) xs

setNoun :: Int -> Program -> Program
setNoun n (Program pointer memory) = Program pointer (set 1 n memory)

setVerb :: Int -> Program -> Program
setVerb n (Program pointer memory) = Program pointer (set 2 n memory)

programMemory :: Program -> Memory
programMemory (Program _ memory) = memory

goalSeek :: Int -> Program -> Maybe (Int, Int)
goalSeek target program = find match parameters
  where
    parameters :: [(Int, Int)]
    parameters = [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]

    match :: (Int, Int) -> Bool
    match = (==) target . flip simulation program
