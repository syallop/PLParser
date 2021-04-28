{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-|
Module      : PLParser.Text
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Parsers that operate on sections of Text.
-}
module PLParser.Text
  ( textIs
  , takeN

  , takeWhile
  , takeWhile1

  , dropWhile
  , dropWhile1

  , token

  , natural
  , whitespace
  )
  where

import PLParser.Parser

import Prelude hiding (takeWhile,dropWhile,exp)

import qualified Data.Text as Text
import Data.Char
import Control.Applicative hiding (optional)
import Data.Text (Text)

import PLParser.Cursor
import PLParser.State
import PLParser.Expected
import PLParser.Combinators

-- | Take an exact string of text from the input.
--
-- If the input does not match, backtracks to the beginning. I.E. this function
-- implicitly 'try's.
textIs
  :: Text
  -> Parser ()
textIs txt = try $ do
  optional (takeN . Text.length $ txt) >>= \case
    Nothing
      -> failing $ ExpectText txt

    Just txt'
      | txt' == txt -> passing ()
      | otherwise   -> failing $ ExpectText txt


-- | Take exactly N characters.
takeN
  :: Int
  -> Parser Text
takeN i = Parser $ \st -> case advanceCursorN i st of
  -- No remaining characters needed
  (st', 0, txt)
    -> Passing st' txt

  -- Took less than the requested number of characters.
  -- Continue.
  (st', remaining, txt)
    -> Halting st' (failing (ExpectN remaining ExpectAnything))
                   ((txt <>) <$> takeN remaining)

-- | Take the longest text that matches a predicate on the characters.
-- Possibly empty.
takeWhile
  :: Predicate Char
  -> Parser Text
takeWhile predicate = Parser $ \st -> case advanceCursorWhile (_predicate predicate) st of
  (st', txt)
    -- If it's the end of input, we can't be sure that we wouldnt want the next
    -- character too.
    | endOfInput (cursor st')
    -> Halting st' (passing txt)
                   ((txt <>) <$> takeWhile predicate)

    | otherwise
    -> Passing st' txt

-- | Takewhile, but must take at least one character => not the empty text.
takeWhile1
  :: Predicate Char
  -> Parser Text
takeWhile1 predicate = Parser $ \st -> case advanceCursorWhile1 (_predicate predicate) st of
  Nothing
    | endOfInput (cursor st)
    -> Halting st (failing takeWhile1Expected) (takeWhile1 predicate)

    | otherwise
    -> Failing st takeWhile1Expected

  Just (st', txt)
    | endOfInput (cursor st')
    -> Halting st' (passing txt)
                   ((txt <>) <$> takeWhile predicate)

    | otherwise
    -> Passing st' txt

  where
    takeWhile1Expected = ExpectPredicate (enhancingLabel "takeWhile1") (Just $ _predicateExpect predicate)


-- | Drop the longest text that matches a predicate on the characters.
-- Possibly empty.
dropWhile
  :: Predicate Char
  -> Parser ()
dropWhile = require . takeWhile

-- | Drop the longest text that matches a predicate on the characters.
-- Must succeed on at least one character.
dropWhile1
  :: Predicate Char
  -> Parser ()
dropWhile1 = require . takeWhile1

-- | Consume whitespace.
whitespace
  :: Parser ()
whitespace = dropWhile (Predicate isSpace (ExpectPredicate (descriptiveLabel "Whitespace") Nothing))

-- | A natural number: zero and positive integers
natural
  :: Parser Int
natural = read . Text.unpack <$> takeWhile1 (Predicate isDigit $ ExpectPredicate (descriptiveLabel "Natural") Nothing)

-- | A token parser consumes any amount of trailing whitespace.
token
  :: Parser a
  -> Parser a
token p = p <* (try whitespace <|> pure ())


