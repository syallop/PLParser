{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric, FlexibleInstances #-}
{-|
Module      : Example.LispLikeSpec
Copyright   : (c) Samuel A. Yallop, 2021
Maintainer  : syallop@gmail.com
Stability   : experimental

This example parses a LISP-like data structure, attempting to exercise recursively nested parsers.
-}
module Example.LispLikeSpec
  ( spec
  )
  where

import Prelude hiding (takeWhile)

import Test

import PLParser
import PLParser.Expected

import PLPrinter

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Data.Functor

import qualified Control.Applicative as Applicative

spec :: Spec
spec = describe "Lisp(like) parsers" $ do
  prop "Symbols" $ do
    symbol <- generate arbitrarySymbol
    (runParser symbolValue . writeLispLike $ symbol)
      `passes` symbol

  prop "Lists" $ do
    list <- generate . sized $ arbitraryList
    (runParser listValue . writeLispLike $ list)
      `passes` list

  prop "Lisp-like" $ do
    lisp <- generate . sized $ arbitraryLispLike
    (runParser lispLike . writeLispLike $ lisp)
      `passes` lisp

  prop "Examples" $ do
    let example = List [List [Symbol "Foo"]]
    (runParser lispLike . writeLispLike $ example)
      `passes` example


-- | Lisp like structure, aiming to test how deeply nested recursive data
-- behaves.
data LispLike
  = Symbol Text
  | List [LispLike]
  deriving (Eq, Generic)

instance Show LispLike where
  show = Text.unpack . writeLispLike

instance Document LispLike where
  document = text. writeLispLike

{- Misc -}

alphaNumeric :: [Char]
alphaNumeric = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']

isAlphaNumeric :: Predicate Char
isAlphaNumeric = Predicate (\c -> elem c alphaNumeric) (ExpectPredicate (descriptiveLabel "ALPHANUMERIC") Nothing)

spaceSeparated :: Parser a -> Parser [a]
spaceSeparated p = (:) <$> p <*> Applicative.many (textIs " " *> p)

{- Parsers -}

symbolValue :: Parser LispLike
symbolValue = Symbol <$> takeWhile1 isAlphaNumeric

-- | Many LispLike elements, enclosed in parenthesis.
listValue :: Parser LispLike
listValue = List <$> alternatives
  [ textIs "()" $> []
  , charIs '(' *> spaceSeparated lispLike <* charIs ')'
  ]

-- | A LispLike thing is a symbol or a list of LispLike things.
lispLike :: Parser LispLike
lispLike = alternatives $
  [ try $ symbolValue
  , listValue
  ]

{- Generators -}

arbitrarySymbol :: Gen LispLike
arbitrarySymbol = (Symbol . Text.pack) <$> elements ["Foo", "Bar", "Baz"]

arbitraryList :: Int -> Gen LispLike
arbitraryList = fmap List . scale (`div` 2) . listOf . arbitraryLispLike . (`div` 2)

-- Small size => more simple values
-- Large size => more complex values
arbitraryLispLike :: Int -> Gen LispLike
arbitraryLispLike size
  | size < 5  = frequency [ (4, arbitrarySymbol)
                          , (1, arbitraryList size)
                          ]
  | otherwise = frequency [ (1, arbitrarySymbol)
                          , (4, arbitraryList size)
                          ]

{- Printers -}

-- | Render a LispLike to a textual representation that _should_ be able to be
-- parsed back by the lispLike Parser.
writeLispLike :: LispLike -> Text
writeLispLike l = case l of
  Symbol txt
    -> writeLispSymbol txt

  List ls
    -> writeLispList ls

writeLispSymbol :: Text -> Text
writeLispSymbol = id

writeLispList :: [LispLike] -> Text
writeLispList ls = mconcat
  [ "("
  , Text.intercalate " " . fmap writeLispLike $ ls
  , ")"
  ]

