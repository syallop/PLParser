{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
  #-}
{-|
Module      : CursorSpec
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PLParser.Cursor
-}
module CursorSpec where

import PLParser.Cursor

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances

spec :: Spec
spec = do
  describe "Pos" $ sequence_
    [it "Number of characters into a line should always be less than or equal to the total characters" pending

    ,describe "should start zeroed" $ sequence_
      [it "in terms of total characters" pending
      ,it "in terms of new lines" pending
      ,it "in terms of characters into a line" pending
      ]

    ,describe "Should increment" $ sequence_
      [it "total characters by n when asked to increment n along text of length > n" pending
      ,it "total characters by m when asked to increment n along text of length m, m < n" pending
      ,it "new lines by the number of new lines when asked to increment across the entire length" pending
      ,it "new lines by the number of new lines passed when asked to increment across some number less than the length" pending
      ,it "number of characters into a line by the length of the line, when the line contains no newlines and asked to increment across the entire length" pending
      ,it "number of character into a line to zero when asked to increment past a newline and no further" pending
      ,it "number of characters into the next line when asked to increment past a newline and some amount past non-newlines" pending
      ]
    ]

  describe "Cursor" $ sequence_
    [it "Should initialise with an empty position and no previous input" pending

    ,describe "when pointing" $ sequence_
      [it "Should render a ----^ pointer string of the correct length somewhere within the output" pending
      ,it "Should render a ----^ pointer string at the correct line in the output" pending
      ,it "Should be identical to the original input when the cursor line is removed" pending
      ]

    ,describe "when incrementing" $ sequence_
      [it "Should return the next character when one is next" pending
      ,it "Should return a newline character when one is next" pending
      ,it "Should produce the entire input when asked to increment across the entire input" pending
      ,it "Should return nothing when asked to increment past the end of the input" pending
      ,it "Should return a different cursor after an increment" pending
      ,it "Should push the character moved past onto the previous stack" pending
      ,it "Should leave the input, minus the leading character in the next pointer" pending
      ]

    ,describe "when incrementing by many characters n at once" $ sequence_
      [it "Should end up at most n characters in" pending
      ,it "Should produce the same output as repeated calls to increment" pending
      ,it "Should produce the same output when the advance n is broken into n=a+b where everything is non-negative" pending
      ,it "Should not change the cursor when moving by 0" pending
      ]

    ,describe "when peeking" $ sequence_
      [it "Should show the next character if one exists" pending
      ,it "Should show nothing when the input is empty" pending
      ]
    ]
