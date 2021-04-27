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

A NIH parser currently for use with an in-progress Programming Language.
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

  , opt
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

  , collectFailures

  , label
  , state
  , withState

  , documentParsing
  ) where

import Prelude hiding (takeWhile,dropWhile,exp)

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Text (Text)
import qualified Data.Map  as Map
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

resetPosition :: State -> Parsing a -> Parsing a
resetPosition initialSt (Parsing finalSt finalResult) =
  let resetTo = total . position . cursor $ initialSt
   in case jump resetTo finalSt of
        Nothing
          -> error "Failed to reset Cursor after 'try' attempt failed."

        Just resetSt
          -> Parsing resetSt finalResult

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
  pure a = Parser $ \st -> Parsing st $ Pass a
  (<*>) = ap

-- | An 'opt'ional parser lifts success/ failure into the return value.
-- I.E. It returns 'Just a' on success and 'Nothing' on failure.
opt
  :: Parser a
  -> Parser (Maybe a)
opt (Parser p) = Parser $ \st -> case p st of
  Passing st' a
    -> Passing st' (Just a)

  Failing st' _e
    -> Passing st' Nothing

  Halting st' done more
    -> Halting st' (opt done) (opt more)

many
  :: Parser a
  -> Parser [a]
many p = do
  r <- opt p
  case r of
    Nothing
      -> pure []

    Just a
      -> do as <- many p
            pure (a : as)

some
  :: Parser a
  -> Parser [a]
some p = (:) <$> p <*> many p

instance Monad Parser where
  -- | Consume nothing to produce the value.
  return a = Parser $ \st -> Passing st a

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


-- TODO: Drop?
-- | Collect failures at the same cursor position, preserving the order of the
-- expectations.
collectFailures
  :: [(Expected, Cursor)]
  -> Map.Map Cursor Expected
collectFailures
  = Map.fromListWith ExpectEither . map (\(v,k) -> (k,v))

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

-- | Pretend no input has been consumed if a parse fails.
--
-- E.G.
-- Input: "ab"
-- Parser: try (charIs 'a' >> charIs 'c') <|> (charIs 'a' >> charIs 'b')
--
-- Should succeed because 'try' causes the left parser to forget it consumed the
-- 'a', allowing the right parser to succeed.
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

-- | Try each parser in succession, backtracking on failure.
alternatives
  :: [Parser a]
  -> Parser a
alternatives = foldr (<|>) mzero
-- TODO: This doesnt backtrack. But should it?

-- | Take a single character.
-- If there are none left, pend more input.
takeChar
  :: Parser Char
takeChar = Parser $ \st -> case advanceCursor st of
  Nothing
    -> Halting st (failing ExpectAnything) takeChar

  Just (st', c)
    -> Passing st' c

-- | If the next character matches the predicate return it. Otherwise, fail
-- (leaving it consumed).
takeCharIf
  :: Predicate Char
  -> Parser Char
takeCharIf predicate = satisfy predicate takeChar

-- | If the next character is equal to the character provided, succeed.
-- Otherwise fail (leaving it consumed).
--
-- Note: This behaves differently to textIs which will backtrack on failure.
charIs
  :: Char
  -> Parser ()
--charIs c = require $ takeCharIf (Predicate (== c) (ExpectText . Text.singleton $ c))
charIs wantCharacter = Parser $ \st -> case advanceCursor st of
  Nothing
    -> Halting st (failing expected) (charIs wantCharacter)

  Just (st', foundCharacter)
    | foundCharacter == wantCharacter
    -> Passing st' ()

    | otherwise
    -> Failing (recordExpectationAt (cursor st') expected st') expected
  where
    expected = ExpectText . Text.singleton $ wantCharacter

upper, lower, digit :: Parser Char
upper = takeCharIf (Predicate isUpper (ExpectPredicate (descriptiveLabel "upper") Nothing)) :: Parser Char
lower = takeCharIf (Predicate isLower (ExpectPredicate (descriptiveLabel "lower") Nothing)) :: Parser Char
digit = takeCharIf (Predicate isDigit (ExpectPredicate (descriptiveLabel "digit") Nothing)) :: Parser Char

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

-- | Take exactly N characters.
takeN
  :: Int
  -> Parser Text
takeN i = Parser $ \st -> case advanceCursorN i st of
  -- No remaining characters needed
  (st', 0, txt)
    -> Passing st' txt

  -- Took less than the requested number of characters.
  -- Continue.
  (st', remaining, txt)
    -> Halting st' (failing (ExpectN remaining ExpectAnything))
                   ((txt <>) <$> takeN remaining)

-- | Take an exact string of text from the input.
--
-- If the input does not match, backtracks to the beginning. I.E. this function
-- implicitly 'try's.
textIs
  :: Text
  -> Parser ()
textIs txt = try $ do
  opt (takeN . Text.length $ txt) >>= \case
    Nothing
      -> failing $ ExpectText txt

    Just txt'
      | txt' == txt -> passing ()
      | otherwise   -> failing $ ExpectText txt

-- | Take the longest text that matches a predicate on the characters.
-- Possibly empty.
takeWhile
  :: Predicate Char
  -> Parser Text
takeWhile predicate = Parser $ \st -> case advanceCursorWhile (_predicate predicate) st of
  (st', txt)
    -- If it's the end of input, we can't be sure that we wouldnt want the next
    -- character too.
    | endOfInput (cursor st')
    -> Halting st' (passing txt)
                   ((txt <>) <$> takeWhile predicate)

    | otherwise
    -> Passing st' txt

-- | Takewhile, but must take at least one character => not the empty text.
takeWhile1
  :: Predicate Char
  -> Parser Text
takeWhile1 predicate = Parser $ \st -> case advanceCursorWhile1 (_predicate predicate) st of
  Nothing
    | endOfInput (cursor st)
    -> Halting st (failing takeWhile1Expected) (takeWhile1 predicate)

    | otherwise
    -> Failing st takeWhile1Expected

  Just (st', txt)
    | endOfInput (cursor st')
    -> Halting st' (passing txt)
                   ((txt <>) <$> takeWhile predicate)

    | otherwise
    -> Passing st' txt

  where
    takeWhile1Expected = ExpectPredicate (enhancingLabel "takeWhile1") (Just $ _predicateExpect predicate)

-- | Drop the longest text that matches a predicate on the characters.
-- Possibly empty.
dropWhile
  :: Predicate Char
  -> Parser ()
dropWhile = require . takeWhile

-- | Drop the longest text that matches a predicate on the characters.
-- Must succeed on at least one character.
dropWhile1
  :: Predicate Char
  -> Parser ()
dropWhile1 = require . takeWhile1

-- | A natural number: zero and positive integers
natural
  :: Parser Int
natural = read . Text.unpack <$> takeWhile1 (Predicate isDigit $ ExpectPredicate (descriptiveLabel "Natural") Nothing)

-- | Consume whitespace.
whitespace
  :: Parser ()
whitespace  = dropWhile (Predicate isSpace (ExpectPredicate (descriptiveLabel "Whitespace") Nothing))

-- | A token parser consumes any amount of trailing whitespace.
token
  :: Parser a
  -> Parser a
token p = p <* (try whitespace <|> pure ())

-- | Between two Parsers is the one we're interested in.
between
  :: Parser ()
  -> Parser a
  -> Parser ()
  -> Parser a
between l p r = l *> p <* r

-- | If a parser fails, label it's expectation.
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

