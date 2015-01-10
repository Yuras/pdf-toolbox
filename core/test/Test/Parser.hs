{-# LANGUAGE OverloadedStrings #-}

module Test.Parser
(
  spec
)
where

import Data.Attoparsec.ByteString

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Parsers.Object

import Test.Hspec

spec :: Spec
spec = do
  describe "parseStr" $ do
    it "should unescape 3-digit character" $ do
      parseOnly parseStr "(hello\\040world)"
        `shouldBe` Right (Str "hello world")

    it "should unescape 2-digit character" $ do
      parseOnly parseStr "(hello\\40world)"
        `shouldBe` Right (Str "hello world")

    it "should unescape 1-digit character" $ do
      parseOnly parseStr "(hello\\0world)"
        `shouldBe` Right (Str "hello\NULworld")
