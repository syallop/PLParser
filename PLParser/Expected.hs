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
module PLParser.Expected where

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
  = ExpectEither Expected Expected        -- Expected either of
  | ExpectOneOf [Text]                    -- Expected any of
  | ExpectPredicate Text (Maybe Expected) -- Failed predicate with label
  | ExpectAnything                        -- Expected anything => got an EOF
  | ExpectN Int Expected                  -- Expected a N repetitions
  | ExpectLabel Text Expected             -- Expected something with a label
  deriving Show

expectNothing :: Expected
expectNothing = ExpectOneOf []

