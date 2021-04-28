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
Module      : PLParser.Combinators
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

"Combinators" - I.E. 'Parsers' which depend upon other 'Parsers' and use them or modify their behavior in some way.

E.G.
- 'many' takes a 'Parser' and repeats it to get 0 or more results/
- 'try' takes a 'Parser' and modifies it's behavior so that it doesn't consume any input if it fails. (This allows the second alternative in '<|>' to be tried).
-}
module PLParser.Combinators
  ( optional
  , try

  , between

  , many
  , some
  , alternatives

  , label
  )
  where

import Prelude hiding (takeWhile,dropWhile,exp)

import Control.Applicative hiding (many, some, optional)
import Control.Monad

import PLParser.Expected
import PLParser.Parser

-- | If a 'Parser' would have failed, instead succeed with 'Nothing'.
optional
  :: Parser a
  -> Parser (Maybe a)
optional (Parser p) = Parser $ \st -> case p st of
  Passing st' a
    -> Passing st' (Just a)

  Failing st' _e
    -> Passing st' Nothing

  Halting st' done more
    -> Halting st' (optional done) (optional more)

-- | If a 'Parser' fails, still fail but reset the input 'Cursor' as if nothing
-- had been consumed.
--
-- This is useful in combination with alternatives like '<\>' which only tries
-- the second parser when the first has not consumed anything.
--
-- E.G.
-- Input: "ab"
-- Parser: (charIs 'a' >> charIs 'c') <|> (charIs 'a' >> charIs 'b')
--
-- Will fail because the first parser consumes the 'a', and the next character
-- isn't a 'c'.
--
-- Using 'try':
-- Parser: try (charIs 'a' >> charIs 'c') <|> (charIs 'a' >> charIs 'b')
--
-- Will still fail in the first parser, but the 'a' will be unconsumed, allowing
-- the second parser to consume it.
try
  :: Parser a
  -> Parser a
try p = state >>= \st -> try' st p
  where
    try' :: State -> Parser a -> Parser a
    try' initialState (Parser p') = Parser $ \st -> case p' st of
      Passing st' a
        -> Passing st' a

      Failing st' e
        -> resetPosition initialState (Failing st' e)

      Halting st' done more
        -> Halting st' (try' initialState done) (try' initialState more)

-- | Zero or more parses.
--
-- This should be equivalent to 'Control.Applicative.many' but _may_ be more
-- efficient or have better diagnostics.
many
  :: Parser a
  -> Parser [a]
many p = optional p >>= \case
  Nothing
    -> pure []

  Just a
    -> do as <- many p
          pure (a : as)

-- | One or more parses.
--
-- This should be equivalent to 'Control.Applicative.some' but _may_ be more
-- efficient or have better diagnostics.
some
  :: Parser a
  -> Parser [a]
some p = (:) <$> p <*> many p

-- | Each 'Parser' in succession (via '<|>'), failing if none pass.
--
-- If backtracking behavior is desired, parses may need to use 'try'.
alternatives
  :: [Parser a]
  -> Parser a
alternatives = foldr (<|>) mzero
-- TODO: This doesnt backtrack. But should it?

-- | Between two Parsers is the one we're interested in.
between
  :: Parser ()
  -> Parser a
  -> Parser ()
  -> Parser a
between l p r = l *> p <* r

-- | If a 'Parser' fails, wrap the direct reason for failure with a 'Label'.
label
  :: Label
  -> Parser a
  -> Parser a
label l (Parser p) = Parser $ \st -> case p st of
  Failing st' e
    -> Failing st' (ExpectLabel l e)

  Passing st' a
    -> Passing st' a

  Halting st' done more
    -> Halting st' (label l done) (label l more)

