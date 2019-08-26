{-# LANGUAGE OverloadedStrings #-}

module Test.Internal.Util
( spec
)
where

import Pdf.Document.Internal.Util

import Test.Hspec

spec :: Spec
spec = describe "Internal.Util" $ do
  describe "decodeTextString" $ do
    context "when string starts with BOM" $ do
      it "decodes it as UTF16BE" $ do
        decodeTextString "\254\255\NULH\NULe\NULl\NULl\NULo\EOT^"
          `shouldBe` Right "Helloў"

    context "when string doesn't start with BOM" $ do
      it "decodes it as PDFDocEncoding string" $ do
        decodeTextString "Hello\130"
          `shouldBe` Right "Hello‡"
