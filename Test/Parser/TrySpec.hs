{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-|
Module      : Parser.TrySpec
Copyright   : (c) Samuel A. Yallop, 2021
Maintainer  : syallop@gmail.com
Stability   : experimental

Test behavior of the 'try' combinator and how it interacts with backtracking in 'Alternative's.

-}
module Parser.TrySpec
  ( spec
  )
  where

import Test
import PLParser

import qualified Data.Text as Text
import Data.Text (Text)

spec :: Spec
spec = describe "Try" $ do
  prop "failures immediately inside a 'try' reset the cursor position" $ \(c :: Char) -> do
    let input = Text.singleton . differentCharacter $ c
        p = try . charIs $ c
    (feed input . start $ p) `fails` ExpectText (Text.singleton c)
    (feed input . start $ p) `leaves` input
    (feed input . start $ p) `located` 0

  prop "failures deeper inside a 'try' reset the cursor position to the beginning" $ \(c0 :: Char) (c1 :: Char) -> do
    let input = Text.pack [c0, differentCharacter c1]
        parser = try $ charIs c0 >> charIs c1
        p = runParser parser input
    p `fails` ExpectText (Text.singleton c1)
    p `leaves` input
    p `located` 0

  prop "failures across a feed reset the cursor to the start (not the last feed)" $ \(c0 :: Char) (c1 :: Char) -> do
    let notC1 = differentCharacter c1
        p = feed (Text.singleton notC1)
          . feed (Text.singleton c0)
          . start
          . try $ charIs c0 >> charIs c1
    p `fails` ExpectText (Text.singleton c1)
    p `leaves` (Text.pack [c0, notC1])
    p `located` 0 -- Not 1

  prop "suceeding parsers are not effected" $ \(text :: Text) -> do
    let p = runParser (textIs text) text
    p `passes` ()
    p `leaves` ""
    p `located` Text.length text

