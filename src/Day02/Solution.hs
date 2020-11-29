module Day02.Solution where

import Data.List
import Text.Parsec
import Text.Parsec.String

part1 :: String -> String
part1 = show . head . programMemory . run . setVerb 2 . setNoun 12 . readProgram

part2 :: String -> String
part2 = id

type Memory = [Int]

data Program = Program Int Memory deriving (Show)

line :: Parser [Int]
line = number `sepBy` (char ',' *> spaces)
  where
    number = read <$> many digit

readProgram :: String -> Program
readProgram input = go (parse line "" input)
  where
    go (Left _) = Program 0 []
    go (Right r) = Program 0 r

showProgram :: Program -> String
showProgram (Program _ memory) = intercalate "," (map show memory)

run :: Program -> Program
run program@(Program pointer memory) = runOpCode (memory !! pointer) program

runOpCode :: Int -> Program -> Program
runOpCode 1 = binary (+)
runOpCode 2 = binary (*)
runOpCode 99 = id
runOpCode opcode = error ("Unknown opcode " ++ show opcode)

binary :: (Int -> Int -> Int) -> Program -> Program
binary fn (Program pointer memory) =
  let addressA = (memory !! (pointer + 1))
      addressB = (memory !! (pointer + 2))
      destination = (memory !! (pointer + 3))
      nextPointer = pointer + 4
      valueA = (memory !! addressA)
      valueB = (memory !! addressB)
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
