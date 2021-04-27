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
import Test.QuickCheck hiding (total)

import Data.Text (Text)
import qualified Data.Text as Text

-- A string of text with no spaces
newtype TokenText = TokenText {_unTokenText :: Text} deriving Show

instance Arbitrary TokenText where
  arbitrary = TokenText <$> spaceLessText

spaceLessText :: Gen Text
spaceLessText = arbitrary `suchThat` (not . Text.any (`elem` [' ','\n']))


spec :: Spec
spec = do
  describe "Positions" $ sequence_
    [ prop "have a total that is greater or equal to the position within the current line" $
       \position -> withinLine position <= total position

    , describe "start with all counts at 0" $ sequence_
      [ it "in terms of total characters"       $ total     startingPosition  `shouldBe` 0
      , it "in terms of new lines"              $ line       startingPosition `shouldBe` 0
      , it "in terms of characters into a line" $ withinLine startingPosition `shouldBe` 0
      ]

    , describe "can be moved past" $ sequence_
        [ prop "a newline" $ \position ->
            let movedPosition = movePast "\n" position
             in and [ total position + 1  == total movedPosition
                    , line  position + 1  == line  movedPosition
                    , withinLine movedPosition == 0
                    ]

        , prop "text without newlines" $ \position tokenText ->
            let txt = _unTokenText tokenText
                characters = Text.length txt
                movedPosition = movePast (_unTokenText tokenText) position
             in and [ total position + characters == total movedPosition
                    , line position == line movedPosition
                    , withinLine position + characters == withinLine movedPosition
                    ]

        , describe "arbitrary text" $ sequence_
            [ prop "with the total increasing by the character count (including new lines)" $ \position text ->
                total position + (Text.length text) == total (movePast text position)

            , prop "with the line count increasing by the number of new newlines" $ \position text ->
                line position + (Text.length . Text.filter (== '\n') $ text) == line (movePast text position)

            , prop "with the count within the line being the characters since the last newline" $ \position text ->
                Text.foldl (\count char -> if char == '\n' then 0 else count + 1) (withinLine position) text == withinLine (movePast text position)
            ]
        ]
    ]

  describe "Cursors" $ sequence_
    [ describe "should initialise" $ sequence_
        [ prop "with an empty position" $ \text ->
            (position . mkCursor $ text) `shouldBe` startingPosition

        , prop "and all text as the remainder" $ \text ->
            (remainder . mkCursor $ text) `shouldBe` text
        ]

    , describe "can be used to draw pointers" $ sequence_
      [ prop "which point at the index within the current line" $ \cursor ->
          (Text.length . (\(_,pointer,_) -> pointer) . point $ cursor) `shouldBe` ((+1) . withinLine . position $ cursor)

      , prop "after the correct number of lines" $ \cursor ->
          (Text.length . Text.filter (== '\n') . (\(before, _, _) -> before) . point $ cursor) `shouldBe` (line . position $ cursor)

      -- Note (before,after) do NOT necessarily equal (prior,remainder) since
      -- the pointer draws the current line up to the next newline.
      , prop "whose before and after fragments are equal to the whole text" $ \cursor ->
          ((\(before,_,after) -> before <> after) . point $ cursor) `shouldBe` (prior cursor <> remainder cursor)

      , prop "for a specific example" $ do
          let (cursor, _, _) = advanceN 5 $ mkCursor $ "abc\ndef\nhij\n"
              (before, pointer, after) = point cursor
          before `shouldBe` "abc\ndef"
          pointer `shouldBe` "-^"
          after `shouldBe` "\nhij\n"
      ]

    , describe "can be advanced" $ sequence_
        [ prop "to return the next character or end of input" $ \cursor ->
            advance cursor `shouldSatisfy` \nextCursor -> case nextCursor of
              Nothing
                -> remainder cursor == ""
              Just (_, char)
                -> char == Text.head (remainder cursor)

        , prop "repeatedly to reconstruct the original input" $ \cursor ->
            let advanceToEnd :: Cursor -> String
                advanceToEnd cursor = case advance cursor of
                  Nothing
                    -> ""
                  Just (cursor', char)
                    -> char : advanceToEnd cursor'
             in ((prior cursor <>) . Text.pack . advanceToEnd $ cursor) == (prior cursor <> remainder cursor)

        , prop "and are never the same afterwards" $ \cursor ->
            (fmap fst . advance $ cursor) /= Just cursor

        , prop "remember the character they returned" $ \cursor ->
            advance cursor `shouldSatisfy` \nextCursor -> case nextCursor of
              Nothing
                -> remainder cursor == ""
              Just (nextCursor, char)
                -> (Text.head . Text.reverse . prior $ nextCursor) == char

        , prop "increasing their total position by one" $ \cursor ->
            advance cursor `shouldSatisfy` \nextCursor -> case nextCursor of
              Nothing
                -> True

              Just (nextCursor, _)
                -> (total . position $ nextCursor) == ((+1) . total . position $ cursor)

        , prop "and increase their line count, and reset if advancing over a new line" $ \cursor ->
            advance cursor `shouldSatisfy` \nextCursor -> case nextCursor of
              Nothing
                -> True
              Just (nextCursor, char)
                | char == '\n'
                -> and [(withinLine . position $ nextCursor) == 0
                       ,(line . position $ nextCursor) == ((+1) . line . position $ cursor)
                       ]

                | otherwise
                -> and [(withinLine . position $ nextCursor) == ((+1) . withinLine . position $ cursor)
                       ,(line . position $ nextCursor) == (line . position $ cursor)
                       ]
        ]

    , describe "can be advanced multiple characters at a time" $ sequence_ $
        let repeatAdvance :: Int -> Cursor -> (Cursor, Int, Text)
            repeatAdvance i c = repeatAdvance' i c ""

            repeatAdvance' :: Int -> Cursor -> Text -> (Cursor, Int, Text)
            repeatAdvance' 0 c text = (c, 0, text)
            repeatAdvance' n c text = case advance c of
              Nothing
                -> (c, n, text)

              Just (c', char)
                -> repeatAdvance' (n-1) c' (text <> Text.singleton char)

         in [ prop "which is the same as calling advance multiple times" $ \cursor positive ->
                advanceN (getPositive positive) cursor == repeatAdvance (getPositive positive) cursor

            , prop "including 0 which does nothing" $ \cursor ->
                advanceN 0 cursor `shouldSatisfy` (\(nextCursor, remaining, consumed) -> and [ nextCursor == cursor
                                                                                             , remaining  == 0
                                                                                             , consumed   == ""
                                                                                             ]
                                                  )

            , prop "advancing in smaller steps is the same as in one step" $ \cursor positive ->
                let n  = getPositive positive
                    xs = [ (a,b) | a <- [0..n], b <- [0..n], a + b == n]
                 in all (\(a,b) -> let (cursorA, remainingA, textA) = advanceN a cursor
                                       (cursorB, remainingB, textB) = advanceN b cursorA
                                    in (cursorB, remainingA + remainingB, textA <> textB) == advanceN n cursor
                        )
                        xs
            ]

    , describe "can be reset" $ do
        prop "from arbitrary points" $ \cursor ->
          let resetCursor = reset cursor
           in (prior cursor <> remainder cursor)
                `shouldBe`
                  (prior resetCursor <> remainder resetCursor)
    ]

