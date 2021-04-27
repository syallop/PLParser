{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Parser.TextSpec where

import Test
import PLParser

import Data.Text (Text)
import qualified Data.Text as Text

spec :: Spec
spec = describe "Text" $ do
  -- Input:  t
  -- Parser: t
  -- Expect: Success because the parser exactly matches the text
  prop "text matches it's exact input" $ \(text :: Text) -> do
    let p = runParser (textIs text) text
    p `passes` ()
    p `leaves` ""
    p `located` (Text.length text)

  -- Input:  tu
  -- Parser: t
  -- Expect: Success with leftovers because the parser matches a prefix of the
  --         total input.
  prop "text matches itself with any trailing text, which is leftover" $ \(text :: Text) (trailing :: Text) -> do
    let p = runParser (textIs text) (text <> trailing)
    p `passes` ()
    p `leaves` trailing
    p `located` (Text.length text)

  -- Input:  ct
  -- Parser: du
  -- Expect: Rejection because the parser does not match a single character.
  --
  -- Edge-case:
  -- - If the input was empty text, the parser would pend instead of failing.
  -- - Expecting empty text always succeeds.
  --
  -- TODO: Test behavior with empty strings as input/ parser & how it interacts
  -- with pending parsers.
  prop "non-empty text fails when the input is different" $ \(SomeText someText) -> do
    let p = runParser (textIs someText) (differentText someText)
    p `fails` ExpectText someText
    p `located` 0 -- differentText always starts with a different character


  prop "text resets the cursor on failure after partial matches" $ do
    let p = runParser (textIs "abc") "abC"
    p `fails` ExpectText "abc"
    p `leaves` "abC"
    p `located` 0

