{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE
    InstanceSigs
  , DeriveFunctor
  , GADTs
  , OverloadedStrings
  , ScopedTypeVariables
  , TupleSections
  #-}
{-|
Module      : PLParser
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A NIH parser currently for use with an in-progress Programming Language.
-}
module PLParser
  ( -- Core parser functions
    ParseResult(..)
  , Parser (..)
  , runParser
  , pFail
  , pSucceed
  , require
  , satisfy
  , try
  , recoverWith
  , alternatives
  , labeled, (<?>)

   -- Functions on characters
  , takeChar
  , Expected (..)
  , Label (..)
  , LabelUse (..)
  , Predicate (..)
  , takeCharIf
  , charIs

   -- Take kinds of character
  , upper
  , lower
  , digit

   -- Functions on Text/ many characters
  , takeN
  , takeNIf
  , textIs
  , takeWhile
  , takeWhile1
  , dropWhile
  , dropWhile1
  , between

   -- Misc
  , natural
  , whitespace

  , remainder
  , parseResult

  , Cursor ()

  , collectFailures
  ) where

import Prelude hiding (takeWhile,dropWhile,exp,pred)

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Function
import Data.Text (Text)
import qualified Data.Map  as Map
import qualified Data.Text as Text

import PLParser.Cursor
import PLParser.Expected

-- | A Parser is a function which takes 'Text' and either fails or produces some 'a' and some leftover 'Text'.
-- Instances for Monad & Applicative sequence Parsers together left-to-right, propogating failure.
-- Instances for MonadPlus & Alternative sequence left-to-right when successful but have backtracking behaviour on failure.
newtype Parser a = Parser {_unParser :: Cursor -> ParseResult a}

-- Add leftovers to failure?
-- - Allows recovery to work
data ParseResult a
  = ParseSuccess a                    Cursor -- Parsed 'a' with leftovers
  | ParseFailure [(Expected, Cursor)] Cursor -- Expected something with leftovers
  deriving (Show, Functor)

-- | Case analysis on a 'ParseResult'.
parseResult
  :: (a -> Cursor -> b)
  -> ([(Expected,Cursor)] -> Cursor -> b)
  -> ParseResult a
  -> b
parseResult sF fF r = case r of
  ParseSuccess a c
    -> sF a c

  ParseFailure failures c
    -> fF failures c

-- | A predicate on some 'a' also describes it's expected values.
data Predicate a
  = Predicate
    {_predicate       :: a -> Bool
    ,_predicateExpect :: Expected -- What does the predicate expect? Could fall back to a simple label
    }

instance Semigroup a => Semigroup (Parser a) where
  pa0 <> pa1 = do
    a0 <- pa0
    a1 <- pa1
    pure (a0 <> a1)

instance Monoid a => Monoid (Parser a) where
  mempty = return mempty

-- fmap over successfully parsed values
instance Functor Parser where
  fmap f (Parser pa) = Parser $ \c0 -> case pa c0 of
    ParseSuccess a c1
      -> ParseSuccess (f a) c1

    ParseFailure failures c
      -> ParseFailure failures c

-- Delegates to Monad instance
instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  -- | Consume nothing to produce the value.
  return = Parser . ParseSuccess

  -- | The Parser must succeed and may consume input. The result and remaining
  -- input is passed into f.
  (Parser pa) >>= f = Parser $ \c0 -> case pa c0 of
    ParseSuccess a c1
      -> let Parser pb = f a
            in pb c1

    ParseFailure failures c1
      -> ParseFailure failures c1

instance MonadFail Parser where
  fail msg = pFail $ ExpectLabel (Label (Text.pack msg) Descriptive) ExpectAnything

-- If the left alternative fails but consumes input, pretend we havnt and try the right alternative
instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Parser where
  -- | The zero parser fails with no expectations.
  mzero = pFail expectNothing

  -- | Try the left parser, if it fails backtrack, trying the right as if no
  -- input had been consumed.
  mplus (Parser pa0) (Parser pa1) = Parser $ \cur0 -> case pa0 cur0 of
    ParseSuccess a cur1
      -> ParseSuccess a cur1

    ParseFailure failures0 cur1
      -- Consumed no input, try the next.
      | on (==) position cur0 cur1
       -> case pa1 cur0 of
            ParseSuccess a cur2
              -> ParseSuccess a cur2

            -- Remember the first parser was also expected.
            ParseFailure failures1 cur2
              -> ParseFailure (failures0<>failures1) cur2

      -- Consumed input. Fail.
      | otherwise
       -> ParseFailure failures0 cur1

-- | Collect failures at the same cursor position, preserving the order of the
-- expectations.
collectFailures
  :: [(Expected, Cursor)]
  -> Map.Map Cursor Expected
collectFailures
  = Map.fromListWith ExpectEither . map (\(v,k) -> (k,v))

-- | Execute a 'Parser' on some input Text, producing a possible result and leftover Text if successful.
runParser
  :: Parser a
  -> Text
  -> ParseResult a
runParser (Parser p) txt = p . mkCursor $ txt


-- | Fail without consuming anything
pFail
  :: Expected
  -> Parser a
pFail e = Parser $ \cur -> ParseFailure [(e,cur)] cur

-- | Succeed without consuming anything
pSucceed
  :: Parser ()
pSucceed = Parser $ ParseSuccess ()

-- | Require a parse must succeed, but any result is discarded.
require
  :: Parser a
  -> Parser ()
require p = p >>= const pSucceed

-- | A parse must succeed and satisfy a predicate.
satisfy
  :: Predicate a
  -> Parser a
  -> Parser a
satisfy pred (Parser f) = Parser $ \cur0 -> case f cur0 of
  ParseSuccess a cur1
    | _predicate pred a -> ParseSuccess a cur1
    | otherwise         -> ParseFailure [(_predicateExpect pred, cur0)] cur1 -- cur1?

  failure
    -> failure

-- | Pretend no input has been consumed if a parse fails.
try
  :: Parser a
  -> Parser a
try (Parser p) = Parser $ \cur0 -> case p cur0 of
  ParseFailure failures _cur1
    -> ParseFailure failures cur0

  r -> r

-- | If a parser fails, recover with the given function, continuing just after
-- the failure position.
recoverWith
  :: ([(Expected, Position)] -> Position -> Parser a)
  -> Parser a
  -> Parser a
recoverWith recover (Parser p) = Parser $ \cur0 -> case p cur0 of
  ParseFailure failures cur1
    -> let Parser p1 = recover (map (\(e,c)->(e,position c)) failures) (position cur1)
          in p1 cur1

  r -> r


-- | Try each parser in succession, backtracking on failure.
alternatives
  :: [Parser a]
  -> Parser a
alternatives = foldr (<|>) mzero

-- | Label a parser. If it fails, the label will appear in the 'Expected'
-- section of the output.
labeled
  :: Label
  -> Parser a
  -> Parser a
labeled label (Parser f) = Parser $ \cur0 -> case f cur0 of
  ParseFailure failures cur1
    -> ParseFailure (map (\(e,c) -> (ExpectLabel label e,c)) failures) cur1

  success
    -> success

-- | Infix 'labeled'.
(<?>) :: Parser a -> Label -> Parser a
(<?>) = flip labeled


-- | Take a single character (if there are any left).
takeChar
  :: Parser Char
takeChar = Parser $ \cur0 -> case advance cur0 of
  Nothing
    -> ParseFailure [(ExpectAnything, cur0)] cur0

  Just (cur1, c)
    -> ParseSuccess c cur1

-- Take a character that must satisfy a predicate
takeCharIf
  :: Predicate Char
  -> Parser Char
takeCharIf pred = satisfy pred takeChar

-- Take a character if it is equal to the one given
charIs
  :: Char
  -> Parser ()
charIs c = require $ takeCharIf (Predicate (== c) (ExpectText . Text.singleton $ c))

upper, lower, digit :: Parser Char
upper = takeCharIf (Predicate isUpper (ExpectPredicate (descriptiveLabel "upper") Nothing)) :: Parser Char
lower = takeCharIf (Predicate isLower (ExpectPredicate (descriptiveLabel "lower") Nothing)) :: Parser Char
digit = takeCharIf (Predicate isDigit (ExpectPredicate (descriptiveLabel "digit") Nothing)) :: Parser Char

-- | Take a number of chars, if the input is long enough.
takeN
  :: Int
  -> Parser Text
takeN i = Parser $ \cur0 -> case advanceN i cur0 of
  -- No remaining characters needed
  (cur1, 0, txt)
    -> ParseSuccess txt cur1

  (_, n, txt)
    -> error $ "takeN: need " <> show n <> " more characters, taken " <> show txt

-- | Take a number of chars if the resulting text passes a predicate.
takeNIf
  :: Predicate Text
  -> Int
  -> Parser Text
takeNIf pred i = satisfy pred $ takeN i

-- | Take a string of text.
textIs
  :: Text
  -> Parser ()
textIs fullTxt = Parser $ \cur0 -> textIs' fullTxt cur0
  where
    textIs' :: Text -> Cursor -> ParseResult ()
    textIs' txt cur0 = case Text.uncons txt of
      Nothing
        -> ParseSuccess () cur0

      Just (t,ts)
        -> case advance cur0 of
             -- End of input but we still need a character
             Nothing
               -> ParseFailure [(ExpectLabel (Label ("In text" <> fullTxt) Enhancing) . ExpectText . Text.cons t $ ts, cur0)] cur0

             Just (cur1, c)
               | c == t    -> textIs' ts cur1
               | otherwise -> ParseFailure [(ExpectLabel (Label ("In text"<>fullTxt) Enhancing) . ExpectText . Text.cons t $ ts,cur1)] cur1

-- | Take the longest text that matches a predicate on the characters.
-- Possibly empty.
takeWhile
  :: (Char -> Bool)
  -> Parser Text
takeWhile pred = Parser $ \cur0 -> let (cur1,txt) = advanceWhile pred cur0
                                    in ParseSuccess txt cur1

-- | Takewhile, but must take at least one character => not the empty text.
takeWhile1
  :: Predicate Char
  -> Parser Text
takeWhile1 pred = Parser $ \cur0 -> case advanceWhile1 (_predicate pred) cur0 of
  Nothing
    -> ParseFailure [(_predicateExpect pred, cur0)] cur0

  Just (cur1, txt)
    -> ParseSuccess txt cur1

-- | Drop the longest text that matches a predicate on the characters.
-- Possibly empty.
dropWhile
  :: (Char -> Bool)
  -> Parser ()
dropWhile = require . takeWhile

-- | Drop the longest text that matches a predicate on the characters.
-- Must succeed on at least one character.
dropWhile1
  :: Predicate Char
  -> Parser ()
dropWhile1 = require . takeWhile1


-- | A natural number: zero and positive integers
natural
  :: Parser Int
natural = read . Text.unpack <$> takeWhile1 (Predicate isDigit $ ExpectPredicate (descriptiveLabel "ISNATURAL") Nothing)

-- | Consume whitespace.
whitespace
  :: Parser ()
whitespace  = dropWhile isSpace

-- | Between two 'Parser's is the one we're interested in.
between
  :: Parser ()
  -> Parser a
  -> Parser ()
  -> Parser a
between l p r = l *> p <* r

