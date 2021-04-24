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
Module      : PLParser.Expected
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Describe what a Parser expected to see at a position.
-}
module PLParser.Expected
  ( Expected (..)

  , Label (..)
  , LabelUse (..)
  , expectNothing
  , descriptiveLabel
  , enhancingLabel

  , stripLabels

  , Expectations ()
  , noExpectations
  , expect
  , expectationCount
  , expectationsList
  , expectationsAt
  )
  where

import Prelude hiding (takeWhile,dropWhile,exp)

import PLParser.Cursor

import PLLabel
import PLPrinter

import Data.Function
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Tuple (swap)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- | Describe what was 'Expected' during a parse that lead to a failure.
data Expected
  -- | Expected either of
  = ExpectEither Expected Expected

  -- | Expected a predicate that is known by a 'Label' to be satisfied .
  | ExpectPredicate Label (Maybe Expected)

  -- | Expected any single character but end of input was found instead.
  | ExpectAnything

  -- | Expected exactly n repetitions.
  | ExpectN Int Expected

  -- | Expected something with a given 'Label'.
  | ExpectLabel Label Expected

  -- | Expected one thing, followed by another.
  | ExpectThen Expected Expected

  -- | Expected to fail.
  | ExpectFail

  -- | Expected exact text.
  | ExpectText Text
  deriving (Eq, Ord, Show)

instance Document Expected where
  document = document . show

-- | We have no expectations and therefore expect to fail.
expectNothing :: Expected
expectNothing = ExpectFail

-- | Strip all enhancing and descriptive labels from an Expectation.
stripLabels :: Expected -> Expected
stripLabels = \case
  ExpectEither e0 e1
    -> ExpectEither (stripLabels e0) (stripLabels e1)

  ExpectPredicate l mExpect
    -> ExpectPredicate l (fmap stripLabels mExpect)

  ExpectAnything
    -> ExpectAnything

  ExpectN i e
    -> ExpectN i (stripLabels e)

  ExpectLabel _l e
    -> stripLabels e

  ExpectThen e0 e1
    -> ExpectThen (stripLabels e0) (stripLabels e1)

  ExpectFail
    -> ExpectFail

  ExpectText txt
    -> ExpectText txt

{- Collections of Expected's -}

-- | Expectations track all assumptions that did not hold at any point of a
-- parse.
newtype Expectations = Expectations {_unExpectations :: Map Position (Set (Expected, Cursor))}
  deriving (Eq)

instance Document Expectations where
  -- TODO: Yikes.
  document = foldl (\acc (c,e) -> mconcat [ acc
                                          , lineBreak
                                          , text "Expected:", lineBreak
                                          , indent1 $ document e
                                          , text "At:", lineBreak
                                          , indent1 $ document c, lineBreak
                                          ]
                   )
                   mempty
           . List.sortBy (\(c0,_) (c1,_) -> compare c1 c0)
           . Map.toList
           . Map.fromListWith ExpectEither
           . fmap swap
           . Set.toList
           . Set.unions
           . Map.elems
           . _unExpectations

instance Show Expectations where
  show = Text.unpack . render . document

instance Semigroup Expectations where
  e0 <> e1 = Expectations $ on (Map.unionWith Set.union) _unExpectations e0 e1

instance Monoid Expectations where
  mempty = Expectations Map.empty

-- | Empty set of expectations.
noExpectations :: Expectations
noExpectations = Expectations Map.empty

-- | Add a new expectation.
expect :: Cursor -> Expected -> Expectations -> Expectations
expect c e (Expectations m) = Expectations . Map.insertWith Set.union (position c) (Set.singleton (e,c)) $ m

-- | Total number of expectations.
expectationCount :: Expectations -> Int
expectationCount = sum . fmap length . Map.elems . _unExpectations

-- | Extract an unordered list of expectations and where they occured.
--
-- This function exists to make migrating from the old plain type easier.
--
-- When we have time, an idea of how the api is used and have settled on an
-- internal representation this will probably disappear.
expectationsList :: Expectations -> [(Expected,Cursor)]
expectationsList = Set.toList . Set.unions . Map.elems . _unExpectations

-- | Restrict Expectations to a particular position.
expectationsAt :: Position -> Expectations -> Expectations
expectationsAt pos (Expectations e) = Expectations $ Map.filterWithKey (\k _v -> k == pos) e

