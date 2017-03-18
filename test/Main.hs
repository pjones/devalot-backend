{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import Test.Hspec

--------------------------------------------------------------------------------
import qualified FeedbackSpec

--------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
  describe "Feedback" FeedbackSpec.spec
