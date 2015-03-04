{-# LANGUAGE OverloadedStrings #-}

module Test.UnicodeCMap
(
  spec,
)
where

import Pdf.Toolbox.Content.UnicodeCMap

import Data.Either
import qualified Data.Map as Map
import qualified Data.ByteString as ByteString
import Test.Hspec

spec :: Spec
spec = describe "UnicodeCMap" $ do
  parseUnicodeCMapSpec

parseUnicodeCMapSpec :: Spec
parseUnicodeCMapSpec = describe "parseUnicodeCMap" $ do
  it "should parse trivial cmap" $ do
    let input = ByteString.concat
          [ "0 begincodespacerange\n" 
          , "0 beginbfchar\n"
          , "0 beginbfrange\n"
          ]
        res = parseUnicodeCMap input
    res `shouldSatisfy` isRight

  it "should parse codespace ranges" $ do
    let input = ByteString.concat
          [ "1 begincodespacerange\n" 
          , "<0000> <FFFF>\n"
          , "0 beginbfchar\n"
          , "0 beginbfrange\n"
          ]
        res = parseUnicodeCMap input
    fmap unicodeCMapCodeRanges res `shouldBe`
      Right [ (ByteString.pack [0, 0], ByteString.pack [255, 255])]

  it "should parse chars" $ do
    let input = ByteString.concat
          [ "0 begincodespacerange\n" 
          , "1 beginbfchar\n"
          , "<3A51> <D840DC3E>\n"
          , "0 beginbfrange\n"
          ]
        res = parseUnicodeCMap input
    fmap unicodeCMapChars res `shouldBe`
      Right (Map.fromList [(14871,"\131134")])

  it "should parse ranges" $ do
    let input = ByteString.concat
          [ "0 begincodespacerange\n" 
          , "0 beginbfchar\n"
          , "1 beginbfrange\n"
          , "<0000> <005E> <0020>\n"
          ]
        res = parseUnicodeCMap input
    fmap unicodeCMapRanges res `shouldBe`
      Right [(0,94,' ')]
