module Practice.ParserSpec (spec) where

import Data.Foldable (for_)
import Practice.Parser
import Test.Hspec
import Text.Parsec (Parsec, anyChar, char, parse)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

runMaybe :: Parsec String () b -> String -> Maybe b
runMaybe parser input = rightToMaybe (run parser input)

spec :: Spec
spec = do
  describe "charAParser" $
    let cases =
          [ ("a", Just 'a'),
            ("ab", Just 'a'),
            ("ba", Nothing)
          ]
        test (input, expected) =
          it ("parses " ++ input) $ do
            runMaybe charAParser input `shouldBe` expected
     in for_ cases test
  describe "anyChar" $
    let cases =
          [ ("a", Just 'a'),
            ("ab", Just 'a'),
            ("ba", Just 'b')
          ]
        test (input, expected) =
          it ("parses " ++ input) $ do
            runMaybe anyChar input `shouldBe` expected
     in for_ cases test
  describe "stringParser" $
    let cases =
          [ ("a", Just "a"),
            ("ab", Just "ab"),
            ("ba", Just "ba")
          ]
        test (input, expected) =
          it ("parses " ++ input) $ do
            runMaybe stringParser input `shouldBe` expected
     in for_ cases test
  describe "wordParser" $ do
    let input = "i like turtles"
     in do
          it ("parses " ++ input) $
            runMaybe wordParser input `shouldBe` Just "i"
          it ("parses " ++ input) $
            runMaybe (wordParser *> wordParser) input `shouldBe` Just ""
          it ("parses " ++ input) $
            runMaybe (wordParser *> char ' ' *> wordParser) input `shouldBe` Just "like"
  describe "secondWordParser" $ do
    it "parses hello world" $
      runMaybe secondWordParser "hello world" `shouldBe` Just "world"
  describe "twoWordParser" $ do
    it "parses hello world" $
      runMaybe twoWordParser "hello world" `shouldBe` Just ["hello", "world"]
  describe "wordsParser" $ do
    let input = "i like turtles"
     in do
          it ("parses " ++ input) $
            runMaybe wordsParser "hello world" `shouldBe` Just ["hello", "world"]

    describe "Simple Expression Parser" $ do
      describe "numberParser" $ do
        it "parses numbers" $ do
          rightToMaybe (parse numberParser "" "1234") `shouldBe` Just 1234
        it "parses numbers but no spaces" $ do
          rightToMaybe (parse numberParser "" "12 34") `shouldBe` Just 12
      describe "operatorParser" $ do
        it "parses '+'" $ do
          rightToMaybe (parse operatorParser "" "+") `shouldBe` Just TAdd
        it "parses '-'" $ do
          rightToMaybe (parse operatorParser "" "-") `shouldBe` Just TSubtract
        it "does not parse numbers" $ do
          rightToMaybe (parse operatorParser "" "1234") `shouldBe` Nothing
      describe "programParser" $ do
        it "parses terminal numbers" $ do
          rightToMaybe (parse programParser "" "1234") `shouldBe` Just (TTerminal 1234)
        it "requires parenthesis" $ do
          rightToMaybe (parse programParser "" "1 + 2") `shouldBe` Nothing
        it "always requires parenthesis" $ do
          rightToMaybe (parse programParser "" "9 + 10 - 11") `shouldBe` Nothing
        it "parses simple expressions with parenthesis" $ do
          rightToMaybe (parse programParser "" "(3+4)") `shouldBe` Just (TNode (TTerminal 3) TAdd (TTerminal 4))
        it "parses simple expressions with parenthesis and spaces" $ do
          rightToMaybe (parse programParser "" "(3 + 4)") `shouldBe` Just (TNode (TTerminal 3) TAdd (TTerminal 4))
        it "fails for mismatched parenthesis" $ do
          rightToMaybe (parse programParser "" "(5 +) 6") `shouldBe` Nothing
        it "fails for unclosed parenthesis" $ do
          rightToMaybe (parse programParser "" "(7 + 8") `shouldBe` Nothing
        it "parses chained expressions with parenthesis" $ do
          rightToMaybe (parse programParser "" "((12+13)-14)")
            `shouldBe` Just
              ( TNode
                  (TNode (TTerminal 12) TAdd (TTerminal 13))
                  TSubtract
                  (TTerminal 14)
              )
        it "parses chained expressions with parenthesis" $ do
          rightToMaybe (parse programParser "" "(15 - (16 - 17))")
            `shouldBe` Just
              ( TNode
                  (TTerminal 15)
                  TSubtract
                  (TNode (TTerminal 16) TSubtract (TTerminal 17))
              )
      describe "evaluateExpression" $
        let cases =
              [ ("(123+(324-456))", Just (-9)),
                ("(15 - (16 - 17))", Just 16),
                ("22", Just 22),
                ("(12 + 14) - 3", Nothing)
              ]
            test (input, expected) =
              it ("evaluates to " ++ (show expected) ++ " given " ++ input) $ do
                evaluateExpression input `shouldBe` expected
         in for_ cases test
