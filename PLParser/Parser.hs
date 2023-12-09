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
Module      : PLParser.Parser
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Core operations on a 'Parser' which:
- Consumes 'Text' as input
- Produces leftovers on successful or unsuccessful parses
- Can be chained together with a 'Monad' interface
- Can try multiple 'Alternatives' when the branches don't consume input (which can be reset explicitly with the 'try' combinator).
- Can be fed input incrementally/ resumed when more input is required to make a decision.
-}
module PLParser.Parser
  ( Parser (..)
  , Result (..)
  , State ()

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

  -- Common combinators
  , require
  , satisfy

  , end

  -- Misc
  , state
  , withState
  , parseResult
  , documentParsing

  , Predicate (..)

  -- Internal/ probably don't use?
  , resetPosition
  )
  where

import Prelude hiding (takeWhile,dropWhile,exp)

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text

import PLParser.Cursor
import PLParser.Diagnostics
import PLParser.Expected
import PLParser.State

import PLPrinter hiding (between)

-- | A Parser is a function which takes 'Text' and either fails or produces some 'a' and some leftover 'Text'.
-- Instances for Monad & Applicative sequence Parsers together left-to-right, propogating failure.
-- Instances for MonadPlus & Alternative sequence left-to-right when successful but have backtracking behaviour on failure.
newtype Parser a = Parser {_unParser :: State -> Parsing a}

-- | An instance of a running parser has:
data Parsing a = Parsing
  { _state  :: State    -- ^ State which describes the position, leftovers, expectations, etc.
  , _result :: Result a -- ^ An intermediate result.
  }
  deriving Functor

-- | A running parser with a 'Pass' result.
pattern Passing :: State -> a -> Parsing a
pattern Passing st a = Parsing st (Pass a)

-- | A running parser with a 'Fail' result.
pattern Failing :: State -> Expected -> Parsing a
pattern Failing st e = Parsing st (Fail e)

-- | A running parser with a 'Halt' result that can be resumed with or without
-- more input.
pattern Halting :: State -> Parser a -> Parser a -> Parsing a
pattern Halting st done more = Parsing st (Halt done more)

-- For the purpose of pattern matching, these sets of patterns cover all of the
-- cases.
{-# COMPLETE Passing, Failing, Halting #-}

-- | The intermediate or final result of a parse.
data Result a
  -- | Pass with a success value.
  = Pass a

  -- | Fail, denoting what was 'Expected'.
  | Fail Expected

  -- | Halt has two alternative continuations and can either:
  -- - Be informed that the end of input has been reached
  -- - Be potentially supplied more input
  | Halt
    { _done :: Parser a
    , _more :: Parser a
    }
  deriving Functor

instance Eq a => Eq (Result a) where
  p == q = case (p,q) of
    (Pass a, Pass a')
      -> a == a'

    (Fail e, Fail e')
      -> e == e'

    (Halt _ _, Halt _ _)
      -> False

    _ -> False

instance Document a => Document (Parsing a) where
  document = documentParsing document

-- | Pretty print the result of a parse.
--
-- The intended audience for the pretty printing is the person supplying the
-- input (rather than the person supplying the parser).
--
-- That said, the internal error model does not (yet) retain enough information
-- to give good messages here, so we currently apply a few heuristics to guess
-- at what information could be relevant.
--
-- In particular:
-- - When we see a descriptive label, we stop describing further.
-- - We assume the deepest failures are relevant.
-- - We assume any failure at the same position we failed at are relevant.
--
-- Because we don't have context on retained expectations its possible/ likely
-- they are not accepted in the current position without earlier changes.
-- The gamble for now is that this is better than nothing and still allows
-- converging to a good parse.
documentParsing :: (a -> Doc) -> Parsing a -> Doc
documentParsing documentA = \case
  Passing st a
    -> mconcat
         [ text "Passing with result:"
         , lineBreak
         , indent1 . documentA $ a
         , let leftovers = remainder . cursor $ st
            in if Text.null leftovers
                 then mempty
                 else mconcat
                        [ lineBreak
                        , text "and leftovers:"
                        , lineBreak
                        , indent1 . rawText $ leftovers
                        ]
         ]

  Halting st _done _more
    -> mconcat
         [ text "Parser has halted, pending more input or a signal that no more input will be supplied."
         , documentCursor . cursor $ st
         ]

  Failing st e
    -> document . failureSummary st $ e

instance Document a => Document (Result a) where
  document r = mconcat $ case r of
    Pass a
      -> [ text "Pass:", lineBreak
         , indent1 $ document a
         ]

    Fail e
      -> [ text "Fail:", lineBreak
         , indent1 $ document e
         ]

    Halt _ _
      -> [ text "Halt:", lineBreak
         ]

instance Document a => Show (Result a) where
  show = Text.unpack . render . document

-- | Case analysis on a 'ParseResult'.
parseResult
  :: State -- ^ Final state
  -> (a -> State -> b)                    -- ^ On Pass
  -> (Expected -> State-> b)              -- ^ On Fail
  -> (Parser a -> Parser a -> State -> b) -- ^ On Halt
  -> Result a -- ^ Result on which to perform case analysis
  -> b
parseResult st onSuccess onFailure onHalting result = case result of
  Pass a
    -> onSuccess a st

  Fail e
    -> onFailure e st

  Halt done more
    -> onHalting done more st

-- | A predicate on some 'a' also describes it's expected values.
data Predicate a
  = Predicate
    {_predicate       :: a -> Bool
    ,_predicateExpect :: Expected -- What does the predicate expect? Could fall back to a simple label
    }



instance Semigroup a => Semigroup (Parser a) where
  pLeft <> pRight = do
    left  <- pLeft
    right <- pRight
    pure (left <> right)

instance Monoid a => Monoid (Parser a) where
  mempty = Parser $ \st -> Passing st mempty

-- fmap over successfully parsed values
instance Functor Parser where
  fmap f (Parser pa) = Parser $ \st -> case pa st of
    Passing st' a
      -> Passing st' $ f a

    Failing st' e
      -> Failing st' e

    Halting st' done more
      -> Halting st' (fmap f done) (fmap f more)

instance Applicative Parser where
  -- | Consume nothing to produce the value.
  pure a = Parser $ \st -> Passing st a
  (<*>) = ap

instance Monad Parser where
  -- | The Parser must succeed and may consume input. The result and remaining
  -- input is passed into f.
  (>>=) :: forall a b. Parser a -> (a -> Parser b) -> Parser b
  (Parser pa) >>= f = Parser $ \st -> case pa st of
    Passing st' a
      -> let Parser pb = f a
          in pb st'

    Failing st' e
      -> Failing st' e

    Halting st' done more
      -> Halting st' (done >>= f) (more >>= f)

instance MonadFail Parser where
  fail msg = failing $ ExpectLabel (Label (Text.pack msg) Descriptive) ExpectAnything

-- If the left alternative fails but consumes input, pretend we havnt and try the right alternative
instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Parser where
  -- | The zero parser fails with no expectations.
  mzero = failing expectNothing

  -- | Try the first parser, if it fails backtrack, trying the second as if no
  -- input had been consumed.
  mplus first second = state >>= \st -> mplus' st first second

    where
      mplus' :: State -> Parser a -> Parser a -> Parser a
      mplus' initialState (Parser firstParser) (Parser secondParser) = Parser $ \st -> case firstParser st of
        -- When the first parser succeeds, the second can be ignored.
        Passing st' a
          -> Passing st' a

        -- When the first parser fails, we can try the second if no input was
        -- consumed. Parsers can pretend they didnt consume input by using 'try'.
        Failing st' e
          -- Cursor has moved. Cannot try the second parser.
          | differentTotalPosition initialState st'
          -> Failing st' e

          -- Cursor has NOT moved. Second parser may be tried.
          | otherwise
          -> let Parsing st'' _fail = resetPosition initialState (Failing st' e)
              in secondParser (recordExpectationAt (cursor st') e $ st'')

        Halting st' done more
          -> Halting st' (mplus' initialState done (Parser secondParser)) (mplus' initialState more (Parser secondParser))

-- | Execute a 'Parser' on some input Text, producing a possible result and leftover Text if successful.
runParser
  :: Parser a
  -> Text
  -> Parsing a
runParser p txt = starve . feed txt . start $ p

-- | Start a 'Parser' without having any input for it yet.
start
  :: Parser a
  -> Parsing a
start (Parser f) = f emptyState

-- | Feed more input to a Parsing.
--
-- If it has already succeeded or failed this will not change the result, but
-- will append more leftovers.
--
-- If the parser is pending then it is supplied the new input.
feed
  :: Text
  -> Parsing a
  -> Parsing a
feed input = \case
  Passing st a
    -> Passing (appendToState input st) a

  Failing st e
    -> Failing (appendToState input st) e

  Halting st _done (Parser more)
    -> more $ appendToState input st

-- | Starve a running parser of input, causing the two pending states
-- Wanting/Needing to transition to Passing/Failing respectively.
starve
  :: Parsing a
  -> Parsing a
starve = \case
  Passing st a
    -> Passing st a

  Failing st e
    -> Failing st e

  Halting st (Parser done) _more
    -> starve $ done st -- TODO: Is a double (or more) halt necessary?

-- | Enter the 'Passing' state without consuming anything.
passing
  :: a
  -> Parser a
passing a = Parser $ \st -> Passing st a

-- | Enter the 'Failing' state without consuming anything.
failing
  :: Expected
  -> Parser a
failing e = Parser $ \st -> Failing (recordExpectationAt (cursor st) e st) e

-- | Enter a 'Hatling' state without consuming anything.
halting
  :: Parser a
  -> Parser a
  -> Parser a
halting done more = Parser $ \st -> Halting st done more

-- | Require a parse must succeed, but any result is discarded.
require
  :: Parser a
  -> Parser ()
require p = p >>= const (passing ())

-- | A parse must succeed and satisfy a predicate.
satisfy
  :: Predicate a
  -> Parser a
  -> Parser a
satisfy predicate (Parser p) = Parser $ \st -> case p st of
  Failing st' e
    -> Failing st' e

  Passing st' a
    | _predicate predicate a
    -> Passing st' a

    -- Remember previous expectations (?)
    | otherwise
    -> Failing (recordExpectationAt (cursor st) (_predicateExpect predicate)
               . recordExpectations (expectations st)
               $ st'
               )
               (_predicateExpect predicate)

  Halting st' done more
    -> Halting st' (satisfy predicate done) (satisfy predicate more)

-- TODO: EOF vs no more input?
-- | Succeeds when there is no input remaining.
end
  :: Parser ()
end = Parser $ \st -> Halting st (Parser $ \st' -> case advanceCursor st' of
                                                     Nothing
                                                       -> Passing st' ()

                                                     Just (st'', _c)
                                                       -> Failing st'' expected
                                 )
                                 (failing expected)
  where
    expected = ExpectPredicate (enhancingLabel "EOF") Nothing


-- | Access the Parsers current State.
state
  :: Parser State
state = Parser $ \st -> Passing st st

-- | Apply a transformation to the Parser State.
withState
  :: (State -> State)
  -> Parser a
  -> Parser a
withState f (Parser p) = Parser $ \st -> p (f st)


--
resetPosition :: State -> Parsing a -> Parsing a
resetPosition initialSt (Parsing finalSt finalResult) =
  let resetTo = total . position . cursor $ initialSt
   in case jump resetTo finalSt of
        Nothing
          -> error "Failed to reset Cursor after 'try' attempt failed."

        Just resetSt
          -> Parsing resetSt finalResult

-- Jump to a given total character, or fail if out of bounds.
jump
  :: Int
  -> State
  -> Maybe State
jump totalCharacters st =
  let (st', n, _) = advanceCursorN totalCharacters . resetCursor $ st
   in if n /= 0
        then Nothing
        else Just st'

