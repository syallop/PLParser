{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Parser.PredicateSpec where

import Prelude hiding (dropWhile)

import Test
import PLParser
import PLParser.Expected

import qualified Data.Text as Text
import Data.Char (isLower,isSpace)

spec :: Spec
spec = describe "Predicates" $ do
  prop "satisfy" $ pending
  prop "takeCharIf" $ pending
  prop "takeWhile" $ pending

  prop "takeWhile1" $ \(c :: Char) positive -> do
    let p = Predicate (== c) $ ExpectPredicate (descriptiveLabel "some exact character") Nothing
        notC = differentCharacter c
        input = mconcat [ Text.replicate (getPositive positive) (Text.singleton c)
                        , Text.singleton notC
                        ]

    runParser (takeWhile1 p) input
      `passes` (Text.replicate (getPositive positive) (Text.singleton c))

    runParser (takeWhile1 p) input
      `leaves` Text.singleton notC

    -- Double check that the output remains ordered correctly.
    -- TODO: Generate test input that ensures this/ split test?
    runParser (takeWhile1 (Predicate isLower (ExpectPredicate (descriptiveLabel "lower") Nothing))) "abcD"
      `passes` "abc"

    runParser (takeWhile1 (Predicate isLower (ExpectPredicate (descriptiveLabel "lower") Nothing))) "abcD"
      `leaves` "D"

    runParser (takeWhile1 (Predicate isLower (ExpectPredicate (descriptiveLabel "lower") Nothing))) "abcD"
      `consumed` "abc"

  prop "dropWhile" $ do
    let p = Predicate isSpace (ExpectPredicate (descriptiveLabel "Whitespace") Nothing)
    runParser (dropWhile p *> textIs "Sup") "Sup" `passes` ()
    runParser (dropWhile p *> textIs "Sup") "Sup" `consumed` "Sup"

    runParser (dropWhile p *> textIs "Sup") "  Sup" `passes` ()
    runParser (dropWhile p *> textIs "Sup") "  Sup" `consumed` "  Sup"

    runParser (dropWhile p *> textIs " Sup") "  Sup" `fails` (ExpectText " Sup")
    runParser (dropWhile p *> textIs " Sup") "  Sup" `consumed` "  "

  prop "dropWhile1" $ do
    let p = Predicate isSpace (ExpectPredicate (descriptiveLabel "Whitespace") Nothing)
    runParser (dropWhile1 p *> textIs "Sup") " Sup" `passes` ()
    runParser (dropWhile1 p *> textIs "Sup") " Sup" `consumed` " Sup"

    runParser (dropWhile1 p *> textIs "Sup") "  Sup" `passes` ()
    runParser (dropWhile1 p *> textIs "Sup") "  Sup" `consumed` "  Sup"

    runParser (dropWhile1 p *> textIs "Sup") "Sup" `fails` (ExpectPredicate (enhancingLabel "takeWhile1") $ Just $ ExpectPredicate (descriptiveLabel "Whitespace") Nothing)
    runParser (dropWhile1 p *> textIs "Sup") "Sup" `consumed` ""

