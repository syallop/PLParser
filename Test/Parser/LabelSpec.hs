{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-|
Module      : Parser.LabelSpec
Copyright   : (c) Samuel A. Yallop, 2021
Maintainer  : syallop@gmail.com
Stability   : experimental

Test behavior of 'Label's with 'Parser's failure messages.

-}
module Parser.LabelSpec
  ( spec
  )
  where

import Test
import PLParser

spec :: Spec
spec = describe "Label" $ do
  prop "Failing parsers can be labelled" $ do
    runParser (label (Label "enhance" Enhancing) (failing ExpectAnything :: Parser Int)) ""
      `fails` ExpectLabel (Label "enhance" Enhancing) ExpectAnything

    runParser (label (Label "descriptive" Descriptive) (failing ExpectAnything :: Parser Int)) ""
      `fails` ExpectLabel (Label "descriptive" Descriptive) ExpectAnything

    runParser (label (Label "enhance" Enhancing) (halting (failing ExpectAnything :: Parser Int) (failing ExpectAnything :: Parser Int))) ""
      `fails` ExpectLabel (Label "enhance" Enhancing) ExpectAnything

    (starve . feed "me" . feed "ignore" . start $ label (Label "enhance" Enhancing) (halting (failing ExpectAnything :: Parser Int) (failing ExpectAnything :: Parser Int)))
      `fails` ExpectLabel (Label "enhance" Enhancing) ExpectAnything

