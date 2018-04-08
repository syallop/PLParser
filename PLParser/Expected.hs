{-# LANGUAGE
    InstanceSigs
  , GADTs
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
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
  )
  where

import Prelude hiding (takeWhile,dropWhile,exp)

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Monoid
import Data.Text (Text)
import qualified Data.List as List
import qualified Data.Text as Text

-- TODO: Abstract Text so that other types like Doc can be used.
data Expected
  = ExpectEither Expected Expected         -- Expected either of
  | ExpectOneOf [Text]                     -- Expected any of
  | ExpectPredicate Label (Maybe Expected) -- Failed predicate with label
  | ExpectAnything                         -- Expected anything => got an EOF
  | ExpectN Int Expected                   -- Expected a N repetitions
  | ExpectLabel Label Expected             -- Expected something with a label
  deriving Show

-- | A Label is some textual description of what a Parser expected at a point.
-- The LabelUse dictates whether the label is intended to be descriptive of the
-- entire Parse past this point or is used to enhance existing labels and Parser
-- descriptions.
--
-- When describing why a Parser failed, in general descriptions should stop at
-- the first Descriptive Label and otherwise chain Enhancing labels together.
-- For debugging purposes, one might still want to see all Labels regardless of
-- type.
data Label = Label
  { _labelText :: Text
  , _labelUse  :: LabelUse
  }
  deriving Show

descriptiveLabel
  :: Text
  -> Label
descriptiveLabel
  txt = Label txt Descriptive

enhancingLabel
  :: Text
  -> Label
enhancingLabel
  txt = Label txt Enhancing

data LabelUse
  = Descriptive
  | Enhancing
  deriving Show

expectNothing :: Expected
expectNothing = ExpectOneOf []

