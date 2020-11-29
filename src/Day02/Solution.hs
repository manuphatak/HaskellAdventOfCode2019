module Day02.Solution where

import Data.List
import Text.Parsec
import Text.Parsec.String

part1 :: String -> String
part1 = show . head . memory . run . setVerb 2 . setNoun 12 . readProgram

part2 :: String -> String
part2 = id

type Memory = [Int]

data Program = Program {instructionPointer :: Int, memory :: Memory} deriving (Show)

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
showProgram Program {instructionPointer = _, memory = mem} = intercalate "," (map show mem)

run :: Program -> Program
run program = runOpCode (memory program !! instructionPointer program) program

runOpCode :: Int -> Program -> Program
runOpCode 1 program =
  let addressA = (memory program !! (instructionPointer program + 1))
      addressB = (memory program !! (instructionPointer program + 2))
      destination = (memory program !! (instructionPointer program + 3))
      valueA = (memory program !! addressA)
      valueB = (memory program !! addressB)
      nextPointer = instructionPointer program + 4
      nextMemory = set destination (valueA + valueB) (memory program)
   in run (Program nextPointer nextMemory)
runOpCode 2 program =
  let addressA = (memory program !! (instructionPointer program + 1))
      addressB = (memory program !! (instructionPointer program + 2))
      destination = (memory program !! (instructionPointer program + 3))
      valueA = (memory program !! addressA)
      valueB = (memory program !! addressB)
      nextPointer = instructionPointer program + 4
      nextMemory = set destination (valueA * valueB) (memory program)
   in run (Program nextPointer nextMemory)
runOpCode 99 program = program
runOpCode _ program = program

set :: Int -> a -> [a] -> [a]
set position value list = take position list ++ value : drop (position + 1) list

setNoun :: Int -> Program -> Program
setNoun n program = program {memory = set 1 n (memory program)}

setVerb :: Int -> Program -> Program
setVerb n program = program {memory = set 2 n (memory program)}

-- >>> show . head . memory . run . readProgram $ "1,0,0,0,99"
-- "2"

-- >>> :t set 1 12
-- set 1 12 :: forall a. Num a => [a] -> [a]
