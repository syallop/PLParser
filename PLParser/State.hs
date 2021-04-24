{-# LANGUAGE
    OverloadedStrings
  , RankNTypes
  , DataKinds
  , TypeOperators
  , KindSignatures
  , GADTs
  , PolyKinds #-}
{-|
Module      : PLParser.State
Copyright   : (c) Samuel A. Yallop, 2021
Maintainer  : syallop@gmail.com
Stability   : experimental

Users of the library should not generally interact with this module.
It's main purpose is to encapsulate fiddly state operations used in the
implementation of Parsers. For example, with backtracking functionality it is
easy to rollback to a previous state but forget to retain any appended input,
diagnostics, etc.
-}
module PLParser.State
  ( State ()
  , emptyState

  -- * Access state
  , cursor
  , expectations

  -- * Modify state
  -- ** Expectations
  , recordExpectationAt
  , recordExpectations

  -- ** Cursor
  , appendToState
  , advanceCursor
  , advanceCursorN
  , advanceCursorWhile
  , advanceCursorWhile1

  , resetCursor

  -- * Predicates
  , differentTotalPosition
  , sameTotalPosition
  )
  where

import PLParser.Cursor
import PLParser.Expected

import PLPrinter

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Function

-- | The state of the Parser contains:
-- - A Cursor that tracks it's position within the input
-- - A collection of Expectations that did not hold (potentially including those
--   the parser backtracked from).
data State = State
  { _cursor       :: Cursor
  , _expectations :: Expectations
  }

-- The initial empty state.
emptyState
  :: State
emptyState = State (mkCursor "") mempty
-- TODO: Is there a meaningful way to combine Cursors? If so, this could be
-- defined through mempty.

-- | The current Cursor (containing the position and input).
cursor
  :: State
  -> Cursor
cursor = _cursor

-- | Call 'advance' on the 'Cursor'.
advanceCursor
  :: State
  -> Maybe (State, Char)
advanceCursor st = case advance . cursor $ st of
  Nothing
    -> Nothing

  Just (endCursor, c)
    -> Just (st{_cursor = endCursor}, c)

-- | Call 'advanceN' on the 'Cursor'.
advanceCursorN
  :: Int
  -> State
  -> (State, Int, Text)
advanceCursorN i st =
  let (c, remaining, txt) = advanceN i (cursor st)
   in (st{_cursor = c}, remaining, txt)

-- | Call 'advanceWhile' on the 'Cursor'.
advanceCursorWhile
  :: (Char -> Bool)
  -> State
  -> (State, Text)
advanceCursorWhile predicate st =
  let (c, txt) = advanceWhile predicate (cursor st)
   in (st{_cursor = c}, txt)

-- | Call 'advanceWhile1' on the 'Cursor'.
advanceCursorWhile1
  :: (Char -> Bool)
  -> State
  -> Maybe (State, Text)
advanceCursorWhile1 predicate st = case advanceWhile1 predicate (cursor st) of
  Nothing
    -> Nothing

  Just (c, txt)
    -> Just (st{_cursor = c}, txt)

-- | Call 'reset' on the 'Cursor'.
resetCursor
  :: State
  -> State
resetCursor st = st{_cursor = reset . cursor $ st}

-- | The set of recorded Expectations, potentially useful for diagnostics.
expectations
  :: State
  -> Expectations
expectations = _expectations

-- | Append more input to the end of the cursor (not the current position).
appendToState
  :: Text
  -> State
  -> State
appendToState additionalInput (State cursor es) = State (appendToCursor additionalInput cursor) es

-- | Record a new expectation at some Cursor.
-- Logically, this _should_ be a prior or current cursor but is not checked.
recordExpectationAt
  :: Cursor
  -> Expected
  -> State
  -> State
recordExpectationAt atCursor expected (State c es) = State c (expect atCursor expected es)

-- | Record a new set of Expectations.
-- Logically they should point at prior or current cursors but this is not
-- checked.
recordExpectations
  :: Expectations
  -> State
  -> State
recordExpectations es (State c e) = State c (e <> es)

instance Document State where
  document (State c e) = mconcat
    [ text "Stopped at:", lineBreak
    , document c, lineBreak
    , text "Expectations:", lineBreak
    , document e
    ]

instance Show State where
  show = Text.unpack . render . document

-- | The underlying Cursor Position is NOT the same.
differentTotalPosition :: State -> State -> Bool
differentTotalPosition st = not . sameTotalPosition st

-- | The underlying Cursor Position is the same.
sameTotalPosition :: State -> State -> Bool
sameTotalPosition = on sameTotal (position . _cursor)

