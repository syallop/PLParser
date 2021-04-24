{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Parser.AlternativeSpec where

import Test
import PLParser
import PLParser.Expected

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Applicative

spec :: Spec
spec = do
  describe "Alternatives empty" $ do
    prop "empty can be used to fail" $ \(text :: Text) -> do
      let p = runParser (empty :: Parser ()) text
      p `fails` expectNothing
      p `leaves` text
      p `located` 0

  describe "Alternatives (<|>)" $ do
    -- Input:  t
    -- Parser: t | u
    -- Expect: Success because the first alternative succeeded
    prop "parsers continue to succeed when a useless alternative is added" $ \(text :: Text) (trailing :: Text) -> do
      let p = runParser (textIs text <|> textIs trailing) text
      p `passes` ()
      p `leaves` ""

    -- Input:  t
    -- Parser: FAIL | t
    -- Expect: Success because first parser consumed no input
    prop "the second parser in an alternative is tried if the first consumes no input" $ \(text :: Text) -> do
      let p = runParser (empty <|> textIs text) text
      p `passes` ()
      p `leaves` ""

    -- Input:  ab
    -- Parser: ac | ab
    -- Expect: Fail because left parser consumed input
    --
    -- Edge-case:
    --   If b,c are empty then the left parser would pend input instead of failing.
    prop "fails when the first parser fails after consuming input" $ \(a :: Char) (b :: Char) -> do
      let input = Text.pack [a,b]
          c     = differentCharacter b

          leftParser  = charIs a *> charIs c
          rightParser = charIs a *> charIs b

          p = runParser (leftParser <|> rightParser) input
      p `fails` ExpectText (Text.singleton c)
      p `located` 2

    -- Input:  ctdu
    -- Parser: ?cteu | ctdu
    -- Expect: Success because while the left parser consumed input and failed,
    --         it backtracks and allows the right parser to succeed.
    --
    -- TODO: This can be implemented more cleanly...
    prop "passes when the first parser consumed input, fails but was wrapped in a 'try', and the second requires some of that input" $
      \(prefixCharacter :: Char) (prefixText :: Text) (suffixCharacter :: Char) (suffixText :: Text) ->
        let prefix = Text.cons prefixCharacter prefixText
            suffix = Text.cons suffixCharacter suffixText

            text = prefix <> suffix
            differentSuffixCharacter = differentCharacter suffixCharacter
            textWithSamePrefixButDifferentSuffix = prefix <> (Text.singleton differentSuffixCharacter) <> suffixText
         in runParser (try (textIs textWithSamePrefixButDifferentSuffix) <|> textIs text) text `passes` ()

    -- Input:  c
    -- Parser: ?d | e
    -- Expect: Failure with expectations from both alternatives because the left
    --         side did not consume input.
    -- TODO: Test isn't testing this, but that the expectation comes from the
    -- right side.
    prop "rejects with expectations from both parsers when both fail without consuming input" $ \(c0 :: Char) ->
      let [c1, c2] = take 2 . differentCharacters $ c0
       in runParser (try (charIs c1) <|> charIs c2) (Text.singleton c0)
            `fails` (ExpectText . Text.singleton $ c2)

    {- Test the 'alternatives' combinator behaves like chains of <|>s -}

    prop "multiple alternatives will fail at the first when input is consumed" $ \(c0 :: Char) -> do
      let [c1, c2, c3] = take 3 . differentCharacters $ c0
          p = alternatives [ charIs c1
                           , charIs c2
                           , charIs c3
                           ]
      runParser p (Text.singleton c0)
        `fails` (ExpectText . Text.singleton $ c1)
      runParser p (Text.singleton c2)
        `fails` (ExpectText . Text.singleton $ c1)
      runParser p (Text.singleton c3)
        `fails` (ExpectText . Text.singleton $ c1)

    prop "multiple alternatives can fail ta the last when previous consuming failures were wrapped in try's" $ \(c0 :: Char) -> do
      let [c1, c2, c3] = take 3 . differentCharacters $ c0
          p = alternatives [ try $ charIs c1
                           , try $ charIs c2
                           , charIs c3
                           ]
      runParser p (Text.singleton c0)
        `fails` (ExpectText . Text.singleton $ c3)

    prop "multiple alternatives can pass at the last when previous consuming failures were wrapped in try's" $ \(c0 :: Char) -> do
      let [c1, c2, c3] = take 3 . differentCharacters $ c0
          p = alternatives [ try $ charIs c1
                           , try $ charIs c2
                           , charIs c3
                           ]
      runParser p (Text.singleton c3)
        `passes` ()

