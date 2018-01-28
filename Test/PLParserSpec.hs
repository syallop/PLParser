{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
  #-}
{-|
Module      : ParserSpec
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PLParser.
-}
module PLParserSpec where

import PLParser
import PLParser.Expected
import Data.Char (ord,chr)
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as Text
import Control.Applicative

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Coerce

spec :: Spec
spec = describe "Parser" $ sequence_
  [ charSpec
  , textSpec
  , altSpec
  , appSpec
  , appSpec
  ]

-- True if the text is parsed successfully to some result, perhaps with some leftovers
parses :: Parser a -> Text -> Bool
parses p txt = parsesSuchThat p txt (\_ _ -> True)

-- True if the text parses successfully to some result and then a predicate succeeds
parsesSuchThat :: Parser a -> Text -> (a -> Cursor -> Bool) -> Bool
parsesSuchThat p txt pred = case runParser p txt of
  ParseFailure _ _
    -> False
  ParseSuccess a c
    -> pred a c

-- A string of text with no spaces
newtype TokenText = TokenText {_unTokenText :: Text} deriving Show

instance Arbitrary TokenText where
  arbitrary = TokenText <$> spaceLessText

spaceLessText :: Gen Text
spaceLessText = arbitrary `suchThat` (not . Text.any (`elem` [' ','\n']))

{- We can generate arbitrary unit parsers -}
instance Arbitrary (Parser ()) where
  arbitrary = oneof [arbitraryChar
                    ,arbitraryText
                    ,arbitraryThen
                    ,arbitraryAlt
                    ]
    where
      arbitraryChar :: Gen (Parser ())
      arbitraryChar = charIs <$> arbitrary

      arbitraryText :: Gen (Parser ())
      arbitraryText = textIs <$> arbitrary

      arbitraryThen :: Gen (Parser ())
      arbitraryThen = (<>) <$> arbitrary <*> arbitrary

      arbitraryAlt :: Gen (Parser ())
      arbitraryAlt = (<|>) <$> arbitrary <*> arbitrary


-- Test the character parsers
charSpec :: Spec
charSpec = describe "Single characters" $ do
  prop "parse as singletons" prop_charParse
  prop "parse with trailing characters" prop_charTrailingParse
  where
    -- A character parses as a singleton string
    prop_charParse :: Char -> Bool
    prop_charParse c =
      charIs c `parses` Text.singleton c

    -- A character parses regardless of trailing text, which is unchanged.
    prop_charTrailingParse :: Char -> Text -> Bool
    prop_charTrailingParse c trailing =
      charIs c `parsesSuchThat` Text.cons c trailing
               $ \() csr
                  -> remainder csr == trailing

-- Test the text parsers
textSpec :: Spec
textSpec = describe "Text" $ do
  prop "exact matches"         prop_textParse
  prop "with trailing text"    prop_textTrailingParse
  prop "appended"              prop_textAppendParse
  where

    -- 'textIs txt' parses 'txt'
    prop_textParse :: Text -> Bool
    prop_textParse txt =
      textIs txt `parses` txt

    -- Text parses with any amount of trailing text, which is unaltered.
    prop_textTrailingParse :: Text -> Text -> Bool
    prop_textTrailingParse txt trailing =
      textIs txt `parsesSuchThat` (txt <> trailing)
                   $ \() csr
                      -> remainder csr == trailing

    -- Text appended between a space is parsed by appending textIs parsers with
    -- a space in between.
    prop_textAppendParse :: TokenText -> TokenText -> Bool
    prop_textAppendParse txt0 txt1 =
      (textIs (coerce txt0) <> textIs " " <> textIs (coerce txt1)) `parses` (coerce txt0 <> " " <> coerce txt1)

-- <|> alternatives
altSpec :: Spec
altSpec = describe "Alternatives (<|>)" $ do
  prop "Adding an alternative after a successful parse still succeeds" prop_trailingAlternatives
  prop "Adding a parser which fails without consuming input before a successful parser still succeeds" prop_backtrackWhenNothingConsumed
  prop "A parser which fails and consumes input before a successful parser should fail" prop_dontBacktrackWhenSomethingConsumed
  where
    prop_trailingAlternatives :: TokenText -> TokenText -> Bool
    prop_trailingAlternatives txt trailing =
      (textIs (coerce txt) <|> textIs (coerce trailing)) `parses` coerce txt

    prop_backtrackWhenNothingConsumed :: TokenText -> Bool
    prop_backtrackWhenNothingConsumed txt =
      (pFail expectNothing <|> textIs (coerce txt)) `parses` coerce txt

    prop_dontBacktrackWhenSomethingConsumed :: (Char,TokenText) -> (Char,TokenText) -> Bool
    prop_dontBacktrackWhenSomethingConsumed (p,ps) (s,ss) =
      let prefix = Text.cons p (coerce ps)
          suffix = Text.cons s (coerce ss)
          txt                  = prefix <> suffix
          txtThatStartsTheSame = prefix <> (Text.singleton $ if s == maxBound then toEnum 0 else succ s) <> suffix
       in not . parses (textIs txtThatStartsTheSame <|> textIs prefix) $ txt

appSpec :: Spec
appSpec = describe "Applicative" $ do
  prop "pure always succeeds" prop_pureSucceeds
  where
    -- TODO: Pick better random values
    prop_pureSucceeds :: TokenText -> () -> Bool
    prop_pureSucceeds txt a = parses (pure a) (coerce txt)


data ParserTest = ParserTest
  {_validInput :: Text
  ,_parsedBy   :: Parser ()
  }

charParserTest :: Gen ParserTest
charParserTest = do
  c <- arbitrary
  return $ ParserTest (Text.pack [c]) (charIs c)

textParserText :: Gen ParserTest
textParserText = do
  txt <- spaceLessText
  return $ ParserTest txt (textIs txt)

instance Arbitrary ParserTest where
  arbitrary = oneof [charParserTest,textParserText]
