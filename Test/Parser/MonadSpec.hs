{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Parser.MonadSpec
  ( spec
  )
  where

import Test
import PLParser

import Data.Char (toUpper)
import qualified Data.Text as Text
import Data.Text (Text)

spec :: Spec
spec = describe "Monad" $ do
  prop "return consumes nothing and always succeeds" $ \(input :: Text) -> do
    let result = runParser (return ()) input
    result `passes` ()
    result `leaves` input
    result `located` 0

  prop "two parsers that succeed, succeed" $ \(c :: Char) -> do
    let result = runParser (charIs c *> pure c >>= pure . toUpper) (Text.singleton c <> "trailing")
    result `passes` toUpper c
    result `leaves` "trailing"
    result `located` 1
    result `consumed` (Text.singleton c)

  prop "when the first parser fails, fails" $ \(c :: Char) -> do
    let d = differentCharacter c
    let result = runParser (charIs c *> pure c >>= pure . toUpper) (Text.singleton d <> "trailing")

    result `fails` (ExpectText . Text.singleton $ c)
    result `leaves` "trailing"
    result `located` 1
    result `consumed` (Text.singleton d)

  prop "when the second parser fails, fails" $ \(c :: Char) -> do
    let result = runParser (charIs c *> pure c >>= \_ -> textIs "not trailing") (Text.singleton c <> "trailing")

    result `fails` (ExpectText "not trailing")
    result `leaves` "trailing"
    result `located` 1
    result `consumed` (Text.singleton c)

