{-# LANGUAGE
    InstanceSigs
  , GADTs
  , OverloadedStrings
  , ScopedTypeVariables
  , TupleSections
  #-}
{-|
Module      : PLParser.Cursor
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

A Cursor is a position within some text.
-}

module PLParser.Cursor where

import Prelude hiding (takeWhile,dropWhile,exp)

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Monoid
import Data.Function
import Data.Text (Text)
import qualified Data.List as List
import qualified Data.Text as Text

import PLParser.Expected

data Pos
  = Pos
    {_posTotal    :: Int -- Total number of characters into a parse
    ,_posLine     :: Int -- Number of new lines
    ,_posLineChar :: Int -- Number characters into line
    }
  deriving Show

instance Eq  Pos where (==)    = on (==)    _posTotal
instance Ord Pos where compare = on compare _posTotal

-- A cursor is a position within some text, where we remember how much text we've passed,
-- how many newlines and how much into the current line we are but not the prior text itself
data Cursor = Cursor
  {_cursorPrev :: [Text] -- chunks of text we've moved past, in reverse order
  ,_cursorNext :: Text   -- cursor is currently pointing to
  ,_cursorPos  :: Pos    -- cache the position within the text as a whole
  }
  deriving Show

instance Eq  Cursor where (==)    = on (==)    _cursorPos
instance Ord Cursor where compare = on compare _cursorPos

remainder :: Cursor -> Text
remainder (Cursor _ next _) = next

point :: Cursor -> (Text,Text,Text)
point (Cursor prev next (Pos _t _l c))
  = let (untilLineEnd,rest) = Text.span (/= '\n') next
      in ( (Text.concat . reverse $ prev) <> untilLineEnd
         , Text.replicate c "-" <> "^"
         , rest
         )

pointTo :: (Pos -> Text) -> Cursor -> Text
pointTo renderPos (Cursor prev next (Pos t l c))
  = let (untilLineEnd,rest) = Text.span (/= '\n') next
       in mconcat [ Text.concat prev, untilLineEnd, "\n"
                  , Text.replicate c "-","^ ","\n"
                  , renderPos (Pos t l c) <> "\n"
                  , rest
                  ]

-- Increment a number of character along a line
incAlongLine :: Int -> Pos -> Pos
incAlongLine i (Pos t l s) = Pos (t+i) l (s+i)

-- Increment to a new line, reseting to position zero within the line.
-- A newline character takes up one total character.
incLine :: Int -> Pos -> Pos
incLine i (Pos t l s) = Pos (t+1) (l+1) 0

-- Increment a position by a string of Text moved past
incPast :: Text -> Pos -> Pos
incPast txt p = case Text.uncons txt of
  Nothing       -> p
  Just (c,txt') -> incPast txt' $ incPastChar c p

incPastChar :: Char -> Pos -> Pos
incPastChar c
  | c == '\n' = incLine 1
  | otherwise = incAlongLine 1

-- Increment the Cursor past the next character (if there is one), returning it
-- TODO: Same as advance..
incCursor :: Cursor -> Maybe (Char,Cursor)
incCursor (Cursor prev next pos) = do
  (c,next') <- Text.uncons next
  Just . (c,) . Cursor (Text.singleton c : prev) next' $ case c of
     _
      | c == '\n' -> incLine      1 pos
      | otherwise -> incAlongLine 1 pos

-- Given a position within some Text, advance the position by dropping any space
-- like characters until the first non-space character.
dropSpaceLikes :: Cursor -> Cursor
dropSpaceLikes (Cursor prev next pos) = case Text.uncons next of
  Nothing -> Cursor prev next pos
  Just (c,next')
    | c == ' '  -> dropSpaceLikes $ Cursor (Text.singleton c : prev) next' $ incAlongLine 1 pos
    | c == '\n' -> dropSpaceLikes $ Cursor (Text.singleton c : prev) next' $ incLine 1 pos
    | otherwise -> Cursor prev next pos

-- | Peek at the next character without removing it.
peekChar
  :: Cursor
  -> Maybe Char
peekChar (Cursor _prev next _pos) = fst <$> Text.uncons next

-- | Advance past the next character, returning it.
advance
  :: Cursor
  -> Maybe (Cursor, Char)
advance (Cursor prev next pos) = case Text.uncons next of
  Nothing
    -> Nothing

  Just (c, txt)
    -> Just (Cursor (Text.singleton c : prev) txt (incPastChar c pos), c)

-- | Advance past exactly N characters, returning the text.
advanceN
  :: Int
  -> Cursor
  -> Maybe (Cursor, Text)
advanceN i (Cursor prev next pos)
  | i < 0     = Nothing
  | i == 0    = Just (Cursor prev next pos, "")
  | otherwise = let (txtL, txtR) = Text.splitAt i next
                 in Just (Cursor (txtL : prev) txtR (incPast txtL pos), txtL)

-- | Advance past the longest text that matches a predicate.
advanceWhile
  :: (Char -> Bool)
  -> Cursor
  -> (Cursor, Text)
advanceWhile pred (Cursor prev next pos) =
  let (txtL, txtR) = Text.span pred next
   in (Cursor (txtL : prev) txtR (incPast txtL pos), txtL)

-- | AdvanceWhile but must take at least one character.
advanceWhile1
  :: (Char -> Bool)
  -> Cursor
  -> Maybe (Cursor, Text)
advanceWhile1 pred cur0 = case advanceWhile pred cur0 of
  (cur1, txt)
    | txt == "" -> Nothing
    | otherwise -> Just (cur1, txt)

