{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Parser.MonadSpec where

import Test
import PLParser
import PLParser.Expected

import Control.Monad
import qualified Data.Text as Text
import Data.Text (Text)

spec :: Spec
spec = describe "Monad" $ do
  prop "return consumes nothing and always succeeds" $ \(input :: Text) -> do
    let p = runParser (return ()) input
    p `passes` ()
    p `leaves` input
    p `located` 0

  -- TODO
