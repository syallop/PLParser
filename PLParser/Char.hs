{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PLParser.Char
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Parsers that operate on single characters.
-}
module PLParser.Char
  ( charIs
  , takeChar
  , takeCharIf

  , upper
  , lower
  , digit
  )
  where

import PLParser.Parser

import Prelude hiding (takeWhile,dropWhile,exp)

import Data.Char
import qualified Data.Text as Text

import PLParser.Expected
import PLParser.State

-- | If the next character is equal to the character provided, succeed.
-- Otherwise fail (leaving it consumed).
--
-- Note: This behaves differently to textIs which will backtrack on failure.
charIs
  :: Char
  -> Parser ()
charIs wantCharacter = Parser $ \st -> case advanceCursor st of
  Nothing
    -> Halting st (failing expected) (charIs wantCharacter)

  Just (st', foundCharacter)
    | foundCharacter == wantCharacter
    -> Passing st' ()

    | otherwise
    -> Failing (recordExpectationAt (cursor st') expected st') expected
  where
    expected = ExpectText . Text.singleton $ wantCharacter

-- | Take a single character.
-- If there are none left, pend more input.
takeChar
  :: Parser Char
takeChar = Parser $ \st -> case advanceCursor st of
  Nothing
    -> Halting st (failing ExpectAnything) takeChar

  Just (st', c)
    -> Passing st' c

-- | If the next character matches the predicate return it. Otherwise, fail
-- (leaving it consumed).
takeCharIf
  :: Predicate Char
  -> Parser Char
takeCharIf predicate = satisfy predicate takeChar


-- | A single upper case character, as given by 'Data.Char.isUpper'.
upper :: Parser Char
upper = takeCharIf (Predicate isUpper (ExpectPredicate (descriptiveLabel "upper") Nothing)) :: Parser Char

-- | A single lower case character, as given by 'Data.Char.isLower'.
lower :: Parser Char
lower = takeCharIf (Predicate isLower (ExpectPredicate (descriptiveLabel "lower") Nothing)) :: Parser Char

-- | A single digit character, as given by 'Data.Char.isDigit'.
digit :: Parser Char
digit = takeCharIf (Predicate isDigit (ExpectPredicate (descriptiveLabel "digit") Nothing)) :: Parser Char

