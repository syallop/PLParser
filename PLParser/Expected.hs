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
import Data.Char
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
  document = documentExpected

-- | Pretty print an 'Expected' that describes why a failure occured
-- (but not where, or the context).
--
-- - Alternatives are rendered as bullet points.
-- - Text is written literally
-- - Descriptive Labels are sufficient to describe a nested failure =>
--   More deeply nested expectations are _not_ rendered.
-- - Enhancing Labels are insufficient to describe a nested failure.
--   More deeply nested expectations _are_ rendered.
documentExpected :: Expected -> Doc
documentExpected = \case
  -- TODO:
  -- This function is _super_ hacky, inefficient and occasionally misleading because
  -- the underlying structure doesnt map well to visualisation. The type needs
  -- re-working or the Parser library should be able to create a better
  -- structured error and perform this fiddly logic.

  -- TODO: Is it better to surround this with quotes (/some other signifier) or
  -- leave unescaped and potentially make things ambiguous (E.G. labels vs exact
  -- text)?
  ExpectText txt
    -> documentText txt

  -- TODO:
  -- - Does this make the output noisy? Is this better as mempty?
  -- - Does this really mean "required input to decide"?
  ExpectAnything
    -> text "Any character"

  -- TODO: Should this report or be empty?
  -- The 'alternatives' function folds with this expectation as a base case.
  -- Does it makes sense for every alternative to report that it expected a
  -- failure?
  -- Is this a rendering mismatch or a bad implementation?
  ExpectFail
    -> text "Nothing after exhausting any (and all) other options."

  ExpectEither eLeft eRight
     -> let runs = List.nub . eitherRuns . ExpectEither eLeft $ eRight
            (simple,_complex) = categorise runs
         in mconcat
              [ bulleted . fmap documentExpected $ simple
              , lineBreak

              --, text "Or less simple alternatives:"
              --, bulleted . fmap ppExpected $ complex
              --, lineBreak
              ]

  -- If a Label is descriptive, we don't need to describe further.
  ExpectLabel (Label label Descriptive) _e
    | label == ""
    -> mempty -- TODO: Should this be interpreted as a request to hide really?

    | otherwise
    -> documentText label

  -- If a Label is enhancing, we'd still like to see the rest of the
  -- Expectation.
  ExpectLabel (Label label Enhancing) e
    | label == ""
    -> mempty

    | otherwise
    -> mconcat
         [ documentText label
         , text ":"
         , lineBreak
         , indent 3 . documentExpected $ e
         ]

  -- If a predicate Label is descriptive, we don't need to describe further.
  ExpectPredicate (Label label Descriptive) _
    | label == ""
    -> mempty -- TODO: Should this be interpreted as a request to hide really?

    | otherwise
    -> mconcat
         [ documentText label
         , text "?"
         ]

  -- If a predicate Label is enhancing, we'd still like to see the rest of the
  -- Expectation.
  ExpectPredicate (Label label Enhancing) mE
    | label == ""
    -> mempty -- TODO: Should this be interpreted as a request to hide really?

    | otherwise
    -> mconcat
         [ documentText label
         , text "?"
         , maybe mempty
                 (\e -> mconcat [ lineBreak
                                , indent 2 . documentExpected $ e
                                ]
                 )
                 mE
         ]

  ExpectN n e
    -> mconcat
         [ text . Text.pack . show $ n
         , text " times:"
         , indent 2 . documentExpected $ e
         ]


  ExpectThen eFirst eThen
    -> mconcat
         [ text "In order:"
         , lineBreak
         , bulleted . filter ((/= "") . render) . fmap documentExpected . thenRuns . ExpectThen eFirst $ eThen
         ]

  where
    -- Break out nested runs of eithers.
    -- E.G. (a | b) | (c | d) = a | b | c | d
    eitherRuns :: Expected -> [Expected]
    eitherRuns = \case
      ExpectEither eLeft eRight
        -> eitherRuns eLeft <> eitherRuns eRight

      e -> [e]

    -- Break out nested runs of thens.
    -- E.G. (a; b); (c; d) = a; b; c; d
    thenRuns :: Expected -> [Expected]
    thenRuns = \case
      ExpectThen eFirst eThen
        -> thenRuns eFirst <> thenRuns eThen

      e -> [e]

    -- Categorise into 'simple' and 'complex'.
    -- Things like expecting literal text, or labels wrapping other simple
    -- things are simple.
    --
    -- Anything else is complex.
    categorise :: [Expected] -> ([Expected], [Expected])
    categorise es = let (simple,complex) = foldl (\(accSimple, accComplex) e
                                                   -> if isSimple e
                                                        then (e:accSimple, accComplex)
                                                        else (accSimple, e:accComplex)
                                                 )
                                                 ([], [])
                                                 es
                     in (reverse simple, reverse complex)
      where
        isSimple :: Expected -> Bool
        isSimple = \case
          ExpectText _
            -> True
          ExpectLabel (Label _ Descriptive) _
            -> True
          ExpectLabel (Label _ Enhancing) e
            -> isSimple e
          ExpectPredicate (Label _ Descriptive) _
            -> True
          ExpectPredicate (Label _ Enhancing) Nothing
            -> True
          ExpectPredicate (Label _ Enhancing) (Just e)
            -> isSimple e


          ExpectN _ _
            -> False
          ExpectAnything
            -> False
          ExpectFail
            -> False
          ExpectEither _ _
            -> False
          ExpectThen _ _
            -> False

-- TODO: Belongs in PLPrinter?

-- | Pretty Print text that may contain non-printable characters.
--
-- If a non-printable character is detected, use escape sequences as given by
-- 'show' but do _not_ surround the result in quotation marks.
--
-- We are therefore making output ambiguous and assuming users can infer from
-- context...
documentText :: Text -> Doc
documentText txt
  | Text.all isPrint txt
  = text txt

  | otherwise
   = text . Text.pack . showUnquoted . Text.unpack $ txt
  where
    -- Show without quotes
    --
    -- Plain 'show':
    -- - Has no quotes for simple characters. E.G.
    --   a
    --   1
    --   @
    -- - Uses escape sequences for other characters, but also surrounds them
    --   with quotes. E.G.
    --   "\n"
    --   "\t"

    -- If a 'Show'able thing is rendered with surrounding quotes, drop them.
    showUnquoted :: Show a => a -> String
    showUnquoted = showUnquoted' . show

    showUnquoted' :: String -> String
    showUnquoted' s@[_]                     = s
    showUnquoted' ('"':s)  | last s == '"'  = init s
                           | otherwise      = s
    showUnquoted' ('\'':s) | last s == '\'' = init s
                           | otherwise      = s
    showUnquoted' s                         = s



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

