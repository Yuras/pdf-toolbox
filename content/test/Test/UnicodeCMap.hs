{-# LANGUAGE OverloadedStrings #-}

module Test.UnicodeCMap
(
  spec,
)
where

import Pdf.Content.UnicodeCMap

import Data.Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.ByteString as ByteString
import Test.Hspec

spec :: Spec
spec = describe "UnicodeCMap" $ do
  parseUnicodeCMapSpec
  unicodeCMapDecodeGlyphSpec
  unicodeCMapNextGlyphSpec

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
      Right (Map.fromList [(14929,"\131134")])

  it "should parse multiple chars" $ do
    let input = ByteString.concat
          [ "0 begincodespacerange\n"
          , "1 beginbfchar\n"
          , "<25> <00660066>\n"
          , "endbfchar\n"
          , "1 beginbfchar\n"
          , "<26> <0066006c>\n"
          , "endbfchar\n"
          , "0 beginbfrange\n"
          ]
        res = parseUnicodeCMap input
    fmap unicodeCMapChars res `shouldBe`
      Right (Map.fromList [(37,"ff"), (38, "fl")])

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

  it "should parse multiple ranges" $ do
    let input = ByteString.concat
          [ "0 begincodespacerange\n"
          , "0 beginbfchar\n"
          , "1 beginbfrange\n"
          , "<21> <21> <0045>\n"
          , "endbfrange\n"
          , "1 beginbfrange\n"
          , "<22> <22> <0073>\n"
          , "endbfrange\n"
          ]
        res = parseUnicodeCMap input
    fmap (List.sort . unicodeCMapRanges) res `shouldBe`
      Right (List.sort [(33,33,'E'), (34, 34, 's')])

  it "should parse array ranges into char map" $ do
    let input = ByteString.concat
          [ "0 begincodespacerange\n"
          , "0 beginbfchar\n"
          , "1 beginbfrange\n"
          , "<005F> <0061> [<00660066> <00660069> <00660066006C>]\n"
          ]
        res = parseUnicodeCMap input
    fmap unicodeCMapChars res `shouldBe`
      Right (Map.fromList [(95,"ff"),(96,"fi"),(97,"ffl")])

  it "should allow spaces in hex numbers" $ do
    let input = ByteString.concat
          [ "1 begincodespacerange\n"
          , "<0000> <FF FF>\n"
          , "0 beginbfchar\n"
          , "0 beginbfrange\n"
          ]
        res = parseUnicodeCMap input
    fmap unicodeCMapCodeRanges res `shouldBe`
      Right [ (ByteString.pack [0, 0], ByteString.pack [255, 255])]

unicodeCMapDecodeGlyphSpec :: Spec
unicodeCMapDecodeGlyphSpec = describe "unicodeCMapDecodeGlyph" $ do
  it "should take glyph from char map if possible" $ do
    let cmap = UnicodeCMap [] charMap []
        charMap = Map.fromList [(42, "hello")]

    let res = unicodeCMapDecodeGlyph cmap 42
    res `shouldBe` Just "hello"

  it "should be able to decode the first glyph from a range" $ do
    let cmap = UnicodeCMap [] mempty [(10, 15, '1')]

    let res = unicodeCMapDecodeGlyph cmap 10
    res `shouldBe` Just "1"

  it "should be able to decode the last glyph from a range" $ do
    let cmap = UnicodeCMap [] mempty [(10, 15, '1')]

    let res = unicodeCMapDecodeGlyph cmap 15
    res `shouldBe` Just "6"

  it "should be able to decode a glyph from a middle of a range" $ do
    let cmap = UnicodeCMap [] mempty [(10, 15, '1')]

    let res = unicodeCMapDecodeGlyph cmap 11
    res `shouldBe` Just "2"

  it "should not decode a glyph before a range" $ do
    let cmap = UnicodeCMap [] mempty [(10, 15, '1')]

    let res = unicodeCMapDecodeGlyph cmap 9
    res `shouldBe` Nothing

  it "should not decode a glyph after a range" $ do
    let cmap = UnicodeCMap [] mempty [(10, 15, '1')]

    let res = unicodeCMapDecodeGlyph cmap 16
    res `shouldBe` Nothing

unicodeCMapNextGlyphSpec :: Spec
unicodeCMapNextGlyphSpec = describe "unicodeCMapNextGlyph" $ do
  it "correctly handles multibyte ranges" $ do
    let cmap = UnicodeCMap [("\0\0", "\1\1")] mempty []
    let Just (code, rest) = unicodeCMapNextGlyph cmap "\1\0rest"
    rest `shouldBe` rest
    code `shouldBe` 256
