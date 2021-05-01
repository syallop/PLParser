{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Parser.ApplicativeSpec
  ( spec
  )
  where

import Test
import PLParser

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (toUpper)
import Data.Functor

import PLPrinter
import Control.Monad

-- Wrap Maybe to avoid declaring orphan instances
newtype Optional a = Optional (Maybe a)
  deriving Eq

instance Document a => Document (Optional a) where
  document (Optional Nothing)  = text "Nothing"
  document (Optional (Just a)) = mconcat
    [ text "Just"
    , document a
    ]

spec :: Spec
spec = describe "Applicative" $ do
  -- Input:  t
  -- Parser: SUCCEED
  -- Expect: Success because the parser always succeeds without looking at
  --         input.
  prop "pure always succeeds" $ \(txt :: Text) ->
    runParser (pure ()) txt `passes` ()

  prop "<*>" $ \(c :: Char) -> do
    runParser (pure toUpper <*> takeChar) (Text.singleton c) `passes` toUpper c
    -- TODO: Test other cases

  -- pass <*> pass = pass
  prop "<*>" $ \(c :: Char) -> do
    runParser (passing toUpper <*> passing c) ""
      `passes` (toUpper c)

  -- pass <*> fail = fail
  prop "<*>" $ do
    runParser (passing toUpper <*> mzero) ""
      `fails` ExpectFail

  -- fail <*> pass = fail
  prop "<*>" $ do
    runParser ((failing ExpectFail :: Parser (() -> ())) <*> passing ()) ""
      `fails` ExpectFail

  -- fail <*> pass = fail
  prop "<*>" $ do
    runParser ((failing ExpectFail :: Parser (() -> ())) <*> mzero) ""
      `fails` ExpectFail

  prop "many" $ \(c :: Char) positive -> do
    let input = Text.replicate (getPositive positive) (Text.singleton c)
        p     = many (charIs c *> pure c)
    runParser p input
      `passes` (replicate (getPositive positive) c)

  prop "many with trailing characters" $ \(c :: Char) positive -> do
    let input = Text.replicate (getPositive positive) (Text.singleton c) <> Text.singleton (differentCharacter c)
        p     = many (charIs c *> pure c)
    runParser p input
      `passes` (replicate (getPositive positive) c)

  prop "many with trailing characters" $ \(c :: Char) positive -> do
    let input = Text.replicate (getPositive positive) (Text.singleton c) <> Text.singleton (differentCharacter c)
        p     = many (charIs c *> pure c)
    runParser p input
      `passes` (replicate (getPositive positive) c)

  prop "many fed input incrementally" $ \(c :: Char) -> do
    --let p = many (charIs c *> pure c)

    -- TODO: For some reason this requires starve to have recursive behavior.
    -- That's only desired behavior if:
    -- - Double halting is fine
    -- - Nested halts always terminate.
    -- Either the double halt behavior should be tested elsewhere
    let notC = differentCharacter c
        p = many . alternatives $ [ try $ charIs notC $> notC
                                  , charIs c    $> c
                                  ]

    (start p)
      `halts` (\_ _ -> pure ())

    (starve . start $ p)
      `passes` []

    runParser p ""
      `passes` []

    (feed (Text.singleton c) . start $ p)
      `halts` (\_ _ -> pure ())

    (starve . feed (Text.singleton c) . start $ p)
      `passes` [c]

    runParser p (Text.singleton c)
      `passes` [c]

    (starve . feed (Text.singleton c) . feed (Text.singleton c) . start $ p)
      `passes` [c, c]

    runParser p (Text.pack [c, c])
      `passes` [c, c]

  prop "opt" $ \(c :: Char) -> do
    let p = fmap Optional . optional . charIs $ c

    -- Halts with no input
    (start p)
      `halts` (\_ _ -> pure ())

    -- Passes with nothing when starved with no input
    (starve . start $ p)
      `passes` (Optional Nothing)

    -- Passes with Just when fed input
    (feed (Text.singleton c) . start $ p)
      `passes` (Optional $ Just ())

