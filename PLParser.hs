{-# LANGUAGE
    InstanceSigs
  , DeriveFunctor
  , GADTs
  , OverloadedStrings
  , ScopedTypeVariables
  , TupleSections
  , PatternSynonyms
  , LambdaCase
  #-}
{-|
Module      : PLParser
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

This module re-exports some core Parser definitions and combinators from nested modules.
-}
module PLParser
  ( -- Core parser functions
    Result (..)
  , State ()
  , Parser (..)

  , Parsing (..)
  , pattern Passing
  , pattern Failing
  , pattern Halting

  -- Parser lifecycle
  , runParser
  , start
  , feed
  , starve

  -- Parses which consume no input and enter the given state.
  , failing
  , passing
  , halting

  , require
  , satisfy
  , try
  , end
  , alternatives

  , optional
  , some
  , many

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
  , textIs
  , takeWhile
  , takeWhile1
  , dropWhile
  , dropWhile1

  , between

   -- Misc
  , natural
  , whitespace
  , token

  , remainder
  , parseResult

  , Cursor ()

  , label
  , state
  , withState

  , documentParsing
  ) where

import Prelude hiding (takeWhile,dropWhile,exp)

import PLParser.Char
import PLParser.Combinators
import PLParser.Cursor
import PLParser.Expected
import PLParser.Parser
import PLParser.Text

