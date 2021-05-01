{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-|
Module      : Parser.HaltingSpec
Copyright   : (c) Samuel A. Yallop, 2021
Maintainer  : syallop@gmail.com
Stability   : experimental

Test behavior of 'Parser's which 'Halt', pending more input.

-}
module Parser.HaltingSpec
  ( spec
  )
  where

import Test
import PLParser

import qualified Data.Text as Text

spec :: Spec
spec = describe "Parsers can halt, pending input" $ do
  -- Input:  EMPTY
  -- Parser: ct
  -- Expect: Pends waiting for input because none was supplied.
  prop "needs more input when supplied none for a text parser" $ \(SomeText someText) ->
    (feed "" . start . textIs $ someText) `halts` (\_done _more -> pure ())

  -- Input:  EMPTY
  -- Parser: ct
  -- Expect: Pends waiting for input because none was supplied.
  --
  -- Input:  ct
  -- Expect: Success because halting parser was supplied matching input.
  prop "when supplied no input, can still later succeed" $ \(SomeText someText) ->
    (feed "" . start . textIs $ someText) `halts` (\_done more -> (feed someText . start $ more) `passes` ())

  -- Input:  EMPTY
  -- Parser: ct
  -- Expect: Pends waiting for input because none was supplied.
  --
  -- Input: dt
  -- Expect: Fails because halting parser was supplied non-matching input.
  prop "when supplied no input, can still later fail" $ \(SomeText someText) ->
    (feed "" . start . textIs $ someText)
      `halts` (\_done more -> feed (differentText someText) (start more)
        `fails` ExpectText someText)

  -- Input:  c
  -- Parser: cd
  -- Expect: Pends waiting for input because some matched but more is expected.
  --
  -- Input d
  -- Expect: Succeeds because halting parser was supplied remaining matching
  --         input
  prop "when supplied some input, can still later succeed" $ \(c0 :: Char) (c1 :: Char) ->
    let text0 = Text.singleton c0
        text1 = Text.singleton c1
        text  = Text.pack [c0,c1]
     in (feed text0 . start . textIs $ text) `halts` (\_done more -> feed text1 (start more) `passes` ())

  -- Input:  c
  -- Parser: cd
  -- Expect: Pends waiting for input because some matched but more is expected.
  --
  -- Input: e
  -- Expect Fails because halting parser was supplied non-matching input.
  prop "when supplied some input, can still later fail" $ \(c0 :: Char) (c1 :: Char) ->
    let text0 = Text.singleton c0
        text1 = Text.singleton c1
        notText1 = Text.singleton . differentCharacter $ c1
        text  = text0 <> text1
     in (feed text0 . start . textIs $ text)
          `halts` (\_done more -> feed notText1 (start more)
            `fails` ExpectText text)

  -- TODO: Test that expectation error messages don't change when broken over a
  --       resume.

