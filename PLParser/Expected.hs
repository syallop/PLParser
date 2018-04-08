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

import PLLabel

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
  | ExpectPredicate Label (Maybe Expected) -- Failed predicate with label
  | ExpectAnything                         -- Expected anything => got an EOF
  | ExpectN Int Expected                   -- Expected a N repetitions
  | ExpectLabel Label Expected             -- Expected something with a label
  | ExpectThen Expected Expected           -- Expected something followed by something
  | ExpectFail                             -- Expected to fail
  | ExpectText Text                        -- Expected exact text
  deriving Show

expectNothing :: Expected
expectNothing = ExpectFail

