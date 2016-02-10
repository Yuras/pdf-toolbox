{-# LANGUAGE OverloadedStrings #-}

module Test.Stream
(
  spec
)
where

import Control.Monad
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import Pdf.Core.Stream

import Test.Hspec

spec :: Spec
spec = describe "Stream" $ do
  describe "readStream" $ do
    it "should throw ParseException when indirect object is not a stream" $ (do
        is <- Streams.fromByteString "1 1 obj\r(hello)\nendobj"
        void $ readStream is 0
      ) `shouldThrow` \(Streams.ParseException _) -> True
