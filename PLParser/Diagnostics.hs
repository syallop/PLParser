{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE
    InstanceSigs
  , GADTs
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , LambdaCase
  , TupleSections
  #-}
{-|
Module      : PLParser.Diagnostics
Copyright   : (c) Samuel A. Yallop, 2021
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exists to take responsibility for the fact that producing good
user-facing error messages for parse failures is currently hard.

For now, these functions suggest _probable_ causes of failure.

Ideally the underlying exception mechanism would both retain more information,
and discard more irrelevant infomation.
-}
module PLParser.Diagnostics
  ( FailureSummary (..)
  , failureSummary
  , documentFailureSummary
  )
  where

import Prelude hiding (takeWhile,dropWhile,exp)

import PLParser.Cursor
import PLParser.Expected
import PLParser.State

import PLPrinter

import Data.Maybe
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | A classification of possible sources of failure that suggest what _might_
-- have caused a failure.
data FailureSummary = FailureSummary
  { -- | What triggered the failure isn't always the root cause once backtracking is involved.
    --
    -- In particular, in a sequence of backtracking alternatives, this will
    -- often be a mempty I.E.
    -- "we expected to fail because we ran out of alternatives."
    --
    -- This isn't particularly useful and the 'real' cause is that all of the
    -- backtracks failed (or perhaps the deepest).
    _finalFailure   :: (Cursor, Expected)

    -- | Anec-data - the deepest failure is often likely to be the 'real' error.
  , _deepestFailures :: Maybe (Cursor, Set Expected)

  -- | All failures that happened to occur at the failure position could be
  -- relevant. Meeting these expectations does not guarantee the parse will
  -- proceed further.
  --
  -- For example, if we:
  -- - Backtrack from a failure at position 1 that terminated at position 2
  -- - Find a matching alternative and proceed to position 2
  -- - Fail all alternatives
  --
  -- Then the first backtrack will be listed here, and satisfying it is _not_
  -- guaranteed to allow the parse to proceed (because it already proceeded past
  -- that point for an unrelated reason).
  --
  , _failuresAtStop :: Set Expected
  }

instance Document FailureSummary where
  document = documentFailureSummary document documentCursor

-- | Given a final failing state and it's expectation, generate a
-- 'FailureSummary' that aims to make sense of the diagnostics collected.
failureSummary
  :: State
  -> Expected
  -> FailureSummary
failureSummary st expected =
  let pointOfFailureCursor :: Cursor
      pointOfFailureCursor = cursor st

      allExpectations :: Expectations
      allExpectations = expectations st

      -- All failures that have been backtracked from, recovered or were involved in the final cause.
      failures :: [(Expected, Cursor)]
      failures = expectationsList allExpectations

      collectFailures :: [(Expected,Cursor)] -> Map.Map Cursor (Set Expected)
      collectFailures allFailures = foldr (\(e,c) acc
                                            -> Map.insertWith (<>) c (Set.singleton e) acc
                                          )
                                          mempty
                                          allFailures

      -- Collect expectations that occur at the same cursor position
      collectedFailures :: Map.Map Cursor (Set Expected)
      collectedFailures = collectFailures failures

      -- Ordered list of expectations, deepest first and excluding the stop
      -- point.
      failuresByDepth :: [(Cursor,Set Expected)]
      failuresByDepth = Map.toDescList $ Map.delete pointOfFailureCursor $ collectedFailures

      -- Expectations at the exact point the error is reported.
      --
      -- These are likely to be backtracks from a related parser but could
      -- equally be leftover from prior state :|
      failureAtStop :: Set Expected
      failureAtStop = fromMaybe Set.empty $ Map.lookup pointOfFailureCursor $ collectedFailures
  in FailureSummary
       { _finalFailure    = (cursor st, expected)
       , _deepestFailures = if failuresByDepth == []
                              then Nothing
                              else let (deepestCursor, deepestExpectations) = minimum failuresByDepth -- TODO: Hmm
                                    in if deepestCursor < pointOfFailureCursor
                                         then Nothing
                                         else Just (deepestCursor, deepestExpectations)
       , _failuresAtStop = failureAtStop
       }

-- | Document a 'FailureSummary' using the provided functions for documenting
-- individual expectations and cursors.
documentFailureSummary
  :: (Expected -> Doc)
  -> (Cursor -> Doc)
  -> FailureSummary
  -> Doc
documentFailureSummary ppExpected ppCursor (FailureSummary (finalCursor, finalFailure) mDeepestFailures failuresAtStop) = mconcat $
  [ text "Parse failure at:"
  , lineBreak, lineBreak
  , indent 2 . mconcat $
      [ ppCursor $ finalCursor
      , lineBreak

      -- TODO: We _might_ be able to generate better errors when we try (and
      -- fail) to match a series of alternatives.
      --
      -- E.G.
      -- alternatives [try p, try q] = try p <|> try q <|> mempty
      --
      -- This is a common top-level pattern and will result in the immediate
      -- reason for failure being that we "expected to fail".
      --
      -- This is kinda misleading and so for now, the hack is to notice this
      -- case and assume any other recorded expectations at the same position
      -- would have allowed the parse to continue and are relevant.
      , if goodFinalFailure
          then mconcat
                 [ text "Expecting:"
                 , lineBreak
                 , indent 2 . ppExpected $ finalFailure
                 ]
          else mempty
      , lineBreak

      -- The deepest failure is often relevant/ the 'real' failure.
      , case mDeepestFailures of
          Nothing
            -> mempty

          Just (deepestCursor, deepestExpectations)
            -> case Set.toList deepestExpectations of
                 []
                   -> mempty

                 [e]
                   -> mconcat
                        [ lineBreak
                        , text "Deepest failure at:"
                        , lineBreak
                        , indent 2 $ mconcat
                            [ ppCursor $ deepestCursor
                            , lineBreak
                            , text "Expecting:"
                            , lineBreak
                            , indent 2 . ppExpected $ e
                            , lineBreak
                            ]
                        ]

                 (e:es)
                   -> mconcat
                        [ lineBreak
                        , text "Deepest failure at:"
                        , lineBreak
                        , indent 2 $ mconcat
                            [ ppCursor $ deepestCursor
                            , lineBreak

                            , text "Expecting:"
                            , lineBreak
                            , indent 2 . ppExpected . foldr ExpectEither e $ es
                            , lineBreak
                            ]
                        ]

     -- Failures that happened to occur at the same character are
     -- potentially useful.
     --
     -- They are _not_ guaranteed to allow parsing to continue due to
     -- how backtracking (badly) interacts with these diagnostics.
     , case Set.toList failuresAtStop of
         []
           -> mempty

         [e]
           -> mconcat $
                [ text "Perhaps we would have allowed:"
                , indent 2 . ppExpected $ e
                , lineBreak
                ]

         (e:es)
           -> mconcat $
                [ text "Perhaps we would have allowed either:"
                , indent 2 . ppExpected . foldr ExpectEither e $ es
                , lineBreak
                ]
     ]
  ]
  where
    goodFinalFailure = finalFailure /= ExpectFail

