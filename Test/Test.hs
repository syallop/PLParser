{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
           , RankNTypes
           , ScopedTypeVariables
  #-}
{-|
Module      : Test
Copyright   : (c) Samuel A. Yallop, 2021
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports helper functions for:
- Asserting the result of parses with semi-readable output
- Asserting properties of parses (position of cursors, leftovers, etc)
- Generating input
- Misc Hspec, quickcheck functions used to write tests.
-}
module Test
  (
  -- * Test Parsing Results
    passes
  , fails
  , halts

  , leaves
  , consumed
  , located

  -- * Generation
  , SomeText (..)
  , differentCharacter
  , differentCharacters
  , differentText

  -- * Re-exports
  -- ** Hspec/ QuickCheck
  , Spec
  , describe
  , it
  , prop
  , shouldBe
  , pending

  , Arbitrary (..)
  , Gen
  , generate
  , sized
  , getSize
  , scale
  , genericShrink

  , elements
  , frequency
  , oneof
  , listOf
  , listOf1
  , getPositive


  -- ** Pretty printing
  , Printer (..)
  , Doc ()
  , textIs
  , lineBreak
  )
  where

import Test.Hspec (Spec, describe, it, pending)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Gen, generate, oneof, elements, frequency, getPositive, listOf, listOf1, sized, getSize, scale, genericShrink)
import Test.Hspec.Expectations

import PLParser
import PLParser.Cursor
import PLParser.State
import PLPrinter

import Data.Text (Text)
import qualified Data.Text as Text

expected :: Doc -> Expectation
expected = expectationFailure . Text.unpack . render

-- | The Parsing result is 'Pass'ing.
passes
  :: (Document a, Eq a)
  => Parsing a
  -> a
  -> Expectation
passes (Parsing st result) expect
  | result == Pass expect
  = pure ()

  | otherwise
  = expected . mconcat $
     [ lineBreak
     , text "Expected:"
     , lineBreak
     , indent1 . document $ Pass expect
     , lineBreak
     , lineBreak

     , text "Actual:"
     , lineBreak
     , indent1 . document $ result
     , lineBreak

     , text "With:"
     , lineBreak
     , indent1 . document $ Parsing st result
     ]

-- | The Parsing result is 'Fail'ing.
fails
  :: forall a
   . (Document a, Eq a)
  => Parsing a
  -> Expected
  -> Expectation
fails (Parsing st result) expect
  | result == Fail expect
  = pure ()

  | otherwise
  = expected . mconcat $
      [ lineBreak
      , text "Expected:"
      , lineBreak
      , indent1 . document $ (Fail expect :: Result a)
      , lineBreak
      , lineBreak

      , text "Actual:"
      , lineBreak
      , indent1 . document $ result
      , lineBreak

      , text "With:"
      , lineBreak
      , indent1 . document $ Parsing st result
      ]


halts
  :: Document a
  => Parsing a
  -> (Parser a -> Parser a -> Expectation)
  -> Expectation
halts (Parsing st result) expectF = case result of
  Halt done more
    -> expectF done more

  _
    -> expected . mconcat $
         [ lineBreak
         , text "Expected to halt in a particular way"
         , lineBreak

         , text "Actual:"
         , lineBreak
         , indent1 . document $ result
         , lineBreak

         , text "With:"
         , lineBreak
         , indent1 . document $ Parsing st result
         ]


-- | Leaves the exact text unconsumed.
leaves
  :: Parsing a
  -> Text
  -> Expectation
leaves (Parsing st _) expectLeftovers = remainder (cursor st) `shouldBe` expectLeftovers

-- | Leaves the exact text consumed before the Cursor.
consumed
  :: Parsing a
  -> Text
  -> Expectation
consumed (Parsing st _) expectPrior = prior (cursor st) `shouldBe` expectPrior

-- | Leaves the cursor pointing at the given total character count
located
  :: Parsing a
  -> Int
  -> Expectation
located (Parsing st _) expectTotal = (total . position . cursor $ st) `shouldBe` expectTotal

{- Generation -}

-- | 'Some' as in Applicatives one or many. I.E. Non-Empty text.
newtype SomeText = SomeText { _someText :: Text }

instance Show SomeText where
  show (SomeText t) = Text.unpack t

instance Arbitrary SomeText where
  arbitrary = (\c txt -> SomeText . Text.cons c $ txt) <$> arbitrary <*> arbitrary

-- | Return a different character.
differentCharacter :: Char -> Char
differentCharacter c = if c == maxBound then toEnum 0 else succ c

-- | All characters that are not an input character.
differentCharacters :: Char -> [Char]
differentCharacters c = filter (/= c) [minBound .. maxBound]

-- | Return different Text.
differentText :: Text -> Text
differentText txt = case Text.uncons txt of
  Nothing
    -> "a"

  Just (c,txt')
    -> Text.cons (differentCharacter c) txt'

