{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-|
Module      : Parser.PredicateSpec
Copyright   : (c) Samuel A. Yallop, 2021
Maintainer  : syallop@gmail.com
Stability   : experimental

Test behavior of the 'Predicate' structure used to parse conditionally of some property.

-}
module Parser.PredicateSpec
  ( spec
  )
  where

import Prelude hiding (dropWhile, takeWhile)

import Test
import PLParser
import PLParser.Expected

import qualified Data.Text as Text
import Data.Char (isLower,isSpace)

spec :: Spec
spec = describe "Predicates" $ do
  prop "satisfy" $ \(c :: Char) -> do
    let e    = ExpectPredicate (descriptiveLabel "some exact character") Nothing
        p    = Predicate (== c) e
        notC = differentCharacter c

    -- Parse succeeds, and predicate succeeds => so does the parser
    let parseAndPredicateSucceeded = runParser (satisfy p takeChar) (Text.singleton c)
    parseAndPredicateSucceeded `passes` c
    parseAndPredicateSucceeded `leaves` ""
    parseAndPredicateSucceeded `consumed` (Text.singleton c)

    -- Parse succeeds and predicate fails => parse fails
    let parseSucceedButPredicateFails = runParser (satisfy p $ takeChar) (Text.singleton notC)
    parseSucceedButPredicateFails `fails` e
    parseSucceedButPredicateFails `leaves` ""
    parseSucceedButPredicateFails `consumed` (Text.singleton notC)

    -- Parse fails but predicate succeeds => parse fails
    let parseFailsButPredicateSucceeds = runParser (satisfy p nope) (Text.singleton c)
    parseFailsButPredicateSucceeds `fails` expectedANope
    parseFailsButPredicateSucceeds `leaves` (Text.singleton c)
    parseFailsButPredicateSucceeds `consumed` ""

    -- Parse fails and predicate fails => parse fails
    let parseAndPredicateFail = runParser (satisfy p nope) (Text.singleton notC)
    parseAndPredicateFail `fails` expectedANope
    parseAndPredicateFail `leaves` (Text.singleton notC)
    parseAndPredicateFail `consumed` ""

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

  -- Test the same things as takeWhile1, but also allow no matches.
  prop "takeWhile" $ \(c :: Char) positive zero -> do
    -- Pick an n that is 0 or positive. Not well distributed...
    let n = if zero then 0 else getPositive positive
        p = Predicate (== c) $ ExpectPredicate (descriptiveLabel "some exact character") Nothing
        notC = differentCharacter c
        input = mconcat [ Text.replicate n (Text.singleton c)
                        , Text.singleton notC
                        ]

    runParser (takeWhile p) input
      `passes` (Text.replicate n (Text.singleton c))

    runParser (takeWhile p) input
      `leaves` Text.singleton notC

    -- Double check that the output remains ordered correctly.
    -- TODO: Generate test input that ensures this/ split test?
    runParser (takeWhile (Predicate isLower (ExpectPredicate (descriptiveLabel "lower") Nothing))) "abcD"
      `passes` "abc"

    runParser (takeWhile (Predicate isLower (ExpectPredicate (descriptiveLabel "lower") Nothing))) "abcD"
      `leaves` "D"

    runParser (takeWhile (Predicate isLower (ExpectPredicate (descriptiveLabel "lower") Nothing))) "abcD"
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

