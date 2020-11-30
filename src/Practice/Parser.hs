module Practice.Parser where

import Text.Parsec

charAParser :: Parsec String st Char
charAParser = char 'a'

run :: Parsec String () a -> String -> Either ParseError a
run parser = parse parser ""

stringParser :: Parsec String st String
stringParser = many anyChar

wordParser :: Parsec String st String
wordParser = many $ noneOf [' ']

secondWordParser :: Parsec String st String
secondWordParser = wordParser *> char ' ' *> wordParser

twoWordParser :: Parsec String st [String]
twoWordParser = listify <$> wordParser <*> (char ' ' *> wordParser)
  where
    listify a b = [a, b]

wordsParser :: Parsec String st [String]
wordsParser = (:) <$> wordParser <*> many (char ' ' *> wordParser)

-- SIMPLE EXPRESSION PARSER
-- https://kunigami.wordpress.com/2014/01/21/an-introduction-to-the-parsec-library/

type TNumber = Int

data TOperator = TAdd | TSubtract deriving (Eq, Show)

data TExpression
  = TNode TExpression TOperator TExpression
  | TTerminal TNumber
  deriving (Eq, Show)

numberParser :: Parsec String st TNumber
numberParser = read <$> many digit

operatorParser :: Parsec String st TOperator
operatorParser = chooseOp <$> oneOf "+-"
  where
    chooseOp '+' = TAdd
    chooseOp '-' = TSubtract
    chooseOp _ = error "this should never happen"

programParser :: Parsec String st TExpression
programParser = expressionParser <* eof

expressionParser :: Parsec String st TExpression
expressionParser = spaces *> expressions <* spaces
  where
    expressions =
      between (char '(') (char ')') binaryExpressionParser
        <|> (TTerminal <$> numberParser)

binaryExpressionParser :: Parsec String st TExpression
binaryExpressionParser = TNode <$> expressionParser <*> operatorParser <*> expressionParser

evaluateExpression :: String -> Maybe Int
evaluateExpression input = go (run programParser input)
  where
    go :: Either ParseError TExpression -> Maybe Int
    go (Right (TNode left TAdd right)) = (+) <$> go (Right left) <*> go (Right right)
    go (Right (TNode left TSubtract right)) = (-) <$> go (Right left) <*> go (Right right)
    go (Right (TTerminal value)) = Just value
    go (Left _) = Nothing
