{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Parser.PredicateSpec where

import Test
import PLParser
import PLParser.Expected

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Char (isLower, isUpper,toUpper,toLower)

spec :: Spec
spec = describe "Predicates" $ do
  prop "satisfy" $ pending
  prop "takeCharIf" $ pending
  prop "takeWhile" $ pending

  prop "takeWhile1" $ \(c :: Char) positive -> do
    let pred = Predicate (== c) $ ExpectPredicate (descriptiveLabel "some exact character") Nothing
        notC = differentCharacter c
        input = mconcat [ Text.replicate (getPositive positive) (Text.singleton c)
                        , Text.singleton notC
                        ]

    runParser (takeWhile1 pred) input
      `passes` (Text.replicate (getPositive positive) (Text.singleton c))

    runParser (takeWhile1 pred) input
      `leaves` Text.singleton notC

    -- Double check that the output remains ordered correctly.
    -- TODO: Generate test input that ensures this/ split test?
    runParser (takeWhile1 (Predicate isLower (ExpectPredicate (descriptiveLabel "lower") Nothing))) "abcD"
      `passes` "abc"

    runParser (takeWhile1 (Predicate isLower (ExpectPredicate (descriptiveLabel "lower") Nothing))) "abcD"
      `leaves` "D"



  prop "takeNIf" $ pending
  prop "dropWhile" $ pending
  prop "dropWhile1" $ pending

