{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Parser.MonoidSpec
  ( spec
  )
  where

import Test
import PLParser

import Data.Functor
import Data.Text (Text)
import qualified Data.Text as Text

spec :: Spec
spec = describe "Monoid" $ do

  -- Input:  t
  -- Parser: SUCCEED
  -- Expect: Success because the first parser trivially succeeds
  prop "mempty always succeeds" $ \(text :: Text) ->
    (feed text $ start (mempty :: Parser Text)) `passes` ""

  prop "<> appends the result of two succeeding parsers" $ \(c0 :: Char) (c1 :: Char) ->
    let input = Text.pack [c0,c1]
        parser = (charIs c0 $> [c0]) <> (charIs c1 $> [c1])
     in (feed input . start $ parser) `passes` [c0,c1]

