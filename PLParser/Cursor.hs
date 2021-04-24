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
  , movePast

  , total
  , line
  , withinLine

  , sameTotal

  , Cursor ()
  , mkCursor
  , appendToCursor

  -- * Access Cursor state
  , remainder
  , prior
  , endOfInput
  , position

  -- * Print visual representation of Cursors
  , point

  -- * Advance the postion of the Cursor.
  , advance
  , advanceN
  , advanceWhile
  , advanceWhile1

  , reset
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

-- | A 'Cursor' is a 'Position' within some 'Text' tracking:
-- - How many characters have been passed
-- - How many newlines have been passed
-- - How many characters have been passed in the current line
data Cursor = Cursor
  { _cursorPriorChunks :: [Text]   -- ^ Chunks of Text that have been moved past, ordered nearest to farthest.
  , _cursorNextChunk   :: Text     -- ^ The next chunk of text. The Cursor is pointing at the first character.
  , _cursorPosition    :: Position -- ^ The position within the Text is cached but should agree with the prior and current chunks.
  }

-- Cursors are equal, regardless of how the prior text is chunked
instance Eq Cursor where
  (Cursor chunks0 next0 pos0) == (Cursor chunks1 next1 pos1) = and
    [ pos0  == pos1
    , next0 == next1
    , mconcat chunks0 == mconcat chunks1
    ]

-- Cursors are ordered, regardless of how the prior text is chunked
instance Ord Cursor where
  compare (Cursor chunks0 next0 pos0) (Cursor chunks1 next1 pos1) =
    compare (pos0, next0, mconcat chunks0) (pos1, next1, mconcat chunks1)

instance Document Cursor where
  document c =
    let (before, pointer, after) = point c
     in mconcat
          [ rawText before, lineBreak
          , text pointer, lineBreak
          , document (_cursorPosition c), lineBreak
          , rawText after
          ]

instance Show Cursor where
  show = Text.unpack . render . document

-- | A Cursor pointing at the first character in a string of Text.
mkCursor :: Text -> Cursor
mkCursor next = Cursor [] next startingPosition

-- | Text before the current character O(n).
prior :: Cursor -> Text
prior (Cursor prev _ _) = Text.reverse . Text.concat $ prev

-- | The remaining chunk of Text.
remainder :: Cursor -> Text
remainder (Cursor _ next _) = next

-- | Is the Cursor at the end of the input/ is the next chunk empty?
endOfInput :: Cursor -> Bool
endOfInput = (== "") . remainder

-- | The position within the Cursors Text describes the total characters as well
-- as the position within lines.
position :: Cursor -> Position
position = _cursorPosition

-- | Append more Text to the end of a Cursor (NOT the current position).
appendToCursor :: Text -> Cursor -> Cursor
appendToCursor t (Cursor priorChunks nextChunk pos) = Cursor priorChunks (nextChunk <> t) pos

point :: Cursor -> (Text,Text,Text)
point (Cursor prev next (Position _t _l c))
  = let (untilLineEnd,rest) = Text.span (/= '\n') next
      in ( (Text.concat . reverse $ prev) <> untilLineEnd
         , Text.replicate c "-" <> "^"
         , rest
         )

{- Movement functions on Positions -}

-- | Move a single character along the current line.
moveAlongLine :: Position -> Position
moveAlongLine (Position t l s) = Position (t+1) l (s+1)

-- | Move to the first position in the next line.
--
-- The newline-character is assumed to take up one line.
moveToStartOfNextLine :: Position -> Position
moveToStartOfNextLine (Position t l _s) = Position (t+1) (l+1) 0

-- | Move past a string of Text, where:
-- - Each character increases the total
-- - '\n' is considered a newline
movePast :: Text -> Position -> Position
movePast txt = case Text.uncons txt of
  Nothing       -> id
  Just (c,txt') -> movePast txt' . movePastChar c

-- | Move past a single Character, where:
-- - Each character increases the total
-- - '\n' is considered a newline
movePastChar :: Char -> Position -> Position
movePastChar c
  | c == '\n' = moveToStartOfNextLine
  | otherwise = moveAlongLine

{- Advancement functions on Cursors -}

-- TODO: Is it reasonable for the two advances to 'fail' in different ways?

-- | Advance past the next character, returning it.
advance
  :: Cursor
  -> Maybe (Cursor, Char)
advance (Cursor prev next pos) = case Text.uncons next of
  Nothing
    -> Nothing

  -- TODO: Might be better to append to prior chunk rather than accumulating
  -- singleton chunks in the pathological case.
  Just (c, txt)
    -> Just (Cursor (Text.singleton c : prev) txt (movePastChar c pos), c)

-- | Advance past N characters, returning the text and the number of characters
-- unable to advance past. I.E. 0 on success.
advanceN
  :: Int
  -> Cursor
  -> (Cursor, Int, Text)
advanceN i (Cursor prev next pos)
  | i < 0     = error "Can't advance backwards"
  | i == 0    = (Cursor prev next pos, 0, "")
  | otherwise = let (txtL, txtR) = Text.splitAt i next
                 in ( Cursor (Text.reverse txtL : prev) txtR (movePast txtL pos)
                    , i - Text.length txtL
                    , txtL
                    )

-- | Advance past the longest text that matches a predicate.
advanceWhile
  :: (Char -> Bool)
  -> Cursor
  -> (Cursor, Text)
advanceWhile pred (Cursor prev next pos) =
  let (txtL, txtR) = Text.span pred next
   in (Cursor (Text.reverse txtL : prev) txtR (movePast txtL pos), txtL)

-- | AdvanceWhile but must take at least one character.
advanceWhile1
  :: (Char -> Bool)
  -> Cursor
  -> Maybe (Cursor, Text)
advanceWhile1 pred cur0 =
  let (cur1, txt) = advanceWhile pred cur0
   in if txt == ""
        then Nothing
        else Just (cur1, txt)

-- | Reset a Cursor to it's starting position.
reset
  :: Cursor
  -> Cursor
reset cursor = mkCursor (prior cursor <> remainder cursor)

