{-# OPTIONS_GHC -Wall #-}
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

module PLParser.Cursor
  ( Position ()
  , startingPosition

  , total
  , line
  , withinLine

  , sameTotal

  , Cursor (..)

  -- * Access Cursor state
  , remainder

  -- * Print visual representation of Cursors
  , point
  , pointTo

  -- * Advance the postion of the Cursor.
  , advance
  , advanceN
  , advanceWhile
  , advanceWhile1

  , incCursor

  -- * Misc
  , peekChar
  )
  where

import Prelude hiding (takeWhile,dropWhile,exp,pred)

import Data.Function
import Data.Text (Text)
import qualified Data.Text as Text

import PLPrinter

-- | A Position within parsing some input counts by overall character as well
-- as tracking by line.
data Position = Position
  { _positionTotal      :: Int -- ^ Total number of characters passed.
  , _positionLine       :: Int -- ^ Number of lines passed.
  , _positionWithinLine :: Int -- ^ Characters within the current line.
  }
  deriving (Show, Eq, Ord)

instance Document Position where
  document (Position t l c) = mconcat
    [ text . Text.pack . show $ t
    , text ":("
    , (text . Text.pack . show $ l)
    , text ","
    , (text . Text.pack . show $ c)
    , text ")"
    ]

-- | The starting position counts 0 characters and lines.
startingPosition :: Position
startingPosition = Position 0 0 0

-- | The total number of characters passed.
total :: Position -> Int
total = _positionTotal

-- | Number of lines passed.
line :: Position -> Int
line = _positionLine

-- | Characters within the current line.
withinLine :: Position -> Int
withinLine = _positionWithinLine

-- | Do two 'Position's contain the same number of Characters, allowing for
-- differences in number of newlines and position within lines?
--
-- This is useful when you _know_ two 'Positions' track the same underlying data
-- and want to do 3x less comparisons...
sameTotal :: Position -> Position -> Bool
sameTotal = on (==) _positionTotal

-- A cursor is a position within some text, where we remember how much text we've passed,
-- how many newlines and how much into the current line we are but not the prior text itself
data Cursor = Cursor
  {_cursorPrev :: [Text] -- chunks of text we've moved past, in reverse order
  ,_cursorNext :: Text   -- cursor is currently pointing to
  ,_cursorPos  :: Position    -- cache the position within the text as a whole
  }
  deriving Show

instance Eq  Cursor where (==)    = on (==)    _cursorPos
instance Ord Cursor where compare = on compare _cursorPos

remainder :: Cursor -> Text
remainder (Cursor _ next _) = next

point :: Cursor -> (Text,Text,Text)
point (Cursor prev next (Position _t _l c))
  = let (untilLineEnd,rest) = Text.span (/= '\n') next
      in ( (Text.concat . reverse $ prev) <> untilLineEnd
         , Text.replicate c "-" <> "^"
         , rest
         )

pointTo :: (Position -> Text) -> Cursor -> Text
pointTo renderPosition (Cursor prev next (Position t l c))
  = let (untilLineEnd,rest) = Text.span (/= '\n') next
       in mconcat [ Text.concat prev, untilLineEnd, "\n"
                  , Text.replicate c "-","^ ","\n"
                  , renderPosition (Position t l c) <> "\n"
                  , rest
                  ]

-- Increment a number of character along a line
incAlongLine :: Int -> Position -> Position
incAlongLine i (Position t l s) = Position (t+i) l (s+i)

-- Increment to a new line, reseting to position zero within the line.
-- A newline character takes up one total character.
incLine :: Position -> Position
incLine (Position t l _s) = Position (t+1) (l+1) 0

-- Increment a position by a string of Text moved past
incPast :: Text -> Position -> Position
incPast txt p = case Text.uncons txt of
  Nothing       -> p
  Just (c,txt') -> incPast txt' $ incPastChar c p

incPastChar :: Char -> Position -> Position
incPastChar c
  | c == '\n' = incLine
  | otherwise = incAlongLine 1

-- Increment the Cursor past the next character (if there is one), returning it
-- TODO: Same as advance..
incCursor :: Cursor -> Maybe (Char,Cursor)
incCursor (Cursor prev next pos) = do
  (c,next') <- Text.uncons next
  Just . (c,) . Cursor (Text.singleton c : prev) next' $ case c of
     _
      | c == '\n' -> incLine        pos
      | otherwise -> incAlongLine 1 pos

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

