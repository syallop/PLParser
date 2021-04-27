{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Parser.CharacterSpec where

import Test
import PLParser

import Data.Text (Text)
import qualified Data.Text as Text

spec :: Spec
spec = describe "Characters" $ do
  -- Input:  c
  -- Parser: c
  -- Expect: Success because the parser exactly matches the character
  prop "singletons" $ \(c :: Char) -> do
    let parser = charIs c
        input  = Text.singleton c
        p = runParser parser input
    p `passes` ()
    p `leaves` ""
    p `located` 1
    p `consumed` input

  -- Input:  ct
  -- Parser: c
  -- Expect: Success with leftovers because the parser matches a prefix of the
  --         total input.
  prop "parse with trailing characters" $ \(c :: Char) (trailing :: Text) -> do
    let p = runParser (charIs c) (Text.cons c trailing)
    p `passes` ()
    p `leaves` trailing
    p `located` 1
    p `consumed` (Text.singleton c)

  -- Input:  c
  -- Parser: d
  -- Expect: Rejection because the parser does not match the character.
  prop "fails when a different character is supplied" $ \(c :: Char) -> do
    let d = differentCharacter c
        p = runParser (charIs c) (Text.singleton d)
    p `fails` ExpectText (Text.singleton c)
    p `leaves` ""
    p `located` 1
    p `consumed` (Text.singleton d)

