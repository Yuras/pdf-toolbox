
-- | Font info contains information, extracted from font,
-- that may be needed when processing content stream

module Pdf.Toolbox.Content.FontInfo
(
  FontInfo(..),
  FISimple(..),
  SimpleFontEncoding(..),
  FIComposite(..),
  CIDFontWidths(..),
  makeCIDFontWidths,
  cidFontGetWidth,
  fontInfoDecodeGlyphs
)
where

import Data.List
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Encoding as Encoding
import qualified Data.Encoding.CP1252 as Encoding
import qualified Data.Encoding.MacOSRoman as Encoding
import Control.Monad

import Pdf.Toolbox.Core

import Pdf.Toolbox.Content.UnicodeCMap
import Pdf.Toolbox.Content.Transform
import Pdf.Toolbox.Content.Processor (Glyph(..))

-- | Font info
data FontInfo
  = FontInfoSimple FISimple
  | FontInfoComposite FIComposite
  deriving (Show)

-- | Font info for simple fonts
data FISimple = FISimple {
  fiSimpleUnicodeCMap :: Maybe UnicodeCMap,
  fiSimpleEncoding :: Maybe SimpleFontEncoding,
  fiSimpleWidths :: Maybe (Int, Int, [Double])  -- ^ FirstChar, LastChar, list of widths
  }
  deriving (Show)

-- | Encoding of simple font
data SimpleFontEncoding
  = SimpleFontEncodingWinAnsi
  | SimpleFontEncodingMacRoman
  deriving (Show)

-- | Font info for Type0 font
data FIComposite = FIComposite {
  fiCompositeUnicodeCMap :: Maybe UnicodeCMap,
  fiCompositeWidths :: CIDFontWidths,
  fiCompositeDefaultWidth :: Double
  }
  deriving (Show)

-- | Glyph widths for CID fonts
data CIDFontWidths = CIDFontWidths {
  cidFontWidthsChars :: Map Int Double,
  cidFontWidthsRanges :: [(Int, Int, Double)]
  }
  deriving (Show)

instance Monoid CIDFontWidths where
  mempty = CIDFontWidths {
    cidFontWidthsChars = mempty,
    cidFontWidthsRanges = mempty
    }
  w1 `mappend` w2 = CIDFontWidths {
    cidFontWidthsChars = cidFontWidthsChars w1 `mappend` cidFontWidthsChars w2,
    cidFontWidthsRanges = cidFontWidthsRanges w1 `mappend` cidFontWidthsRanges w2
    }

-- | Make `CIDFontWidths` from value of \"W\" key in descendant font
makeCIDFontWidths :: Monad m => Array -> PdfE m CIDFontWidths
makeCIDFontWidths (Array vals) = go mempty vals
  where
  go res [] = return res
  go res (ONumber x1 : ONumber x2 : ONumber x3 : xs) = do
    n1 <- intValue x1
    n2 <- intValue x2
    n3 <- realValue x3
    go res {cidFontWidthsRanges = (n1, n2, n3) : cidFontWidthsRanges res} xs
  go res (ONumber x: OArray (Array arr): xs) = do
    n <- intValue x
    ws <- forM arr $ \w -> fromObject w >>= realValue
    go res {cidFontWidthsChars = Map.fromList (zip [n ..] ws) `mappend` cidFontWidthsChars res} xs
  go _ _ = left $ UnexpectedError "Can't parse CIDFont width"

-- | Get glyph width by glyph code
cidFontGetWidth :: CIDFontWidths -> Int -> Maybe Double
cidFontGetWidth w code =
  case Map.lookup code (cidFontWidthsChars w) of
    Just width -> Just width
    Nothing -> case find (\(start, end, _) -> code >= start && code <= end) (cidFontWidthsRanges w) of
                 Just (_, _, width) -> Just width
                 _ -> Nothing

-- | Decode string into list of glyphs and their widths
fontInfoDecodeGlyphs :: FontInfo -> Str -> [(Glyph, Double)]
fontInfoDecodeGlyphs (FontInfoSimple fi) = \(Str bs) ->
  flip map (BS.unpack bs) $ \c ->
    let code = fromIntegral c
        txt =
          case fiSimpleUnicodeCMap fi of
            Nothing ->
              case fiSimpleEncoding fi of
                Nothing ->
                  case Text.decodeUtf8' (BS.pack [c]) of
                    Right t -> Just t
                    _ -> Nothing
                Just SimpleFontEncodingWinAnsi ->
                  case Encoding.decodeStrictByteStringExplicit Encoding.CP1252 (BS.pack [c]) of
                    Left _ -> Nothing
                    Right t -> Just $ Text.pack t
                Just SimpleFontEncodingMacRoman ->
                  case Encoding.decodeStrictByteStringExplicit Encoding.MacOSRoman (BS.pack [c]) of
                    Left _ -> Nothing
                    Right t -> Just $ Text.pack t
            Just toUnicode -> unicodeCMapDecodeGlyph toUnicode code
        width =
          case fiSimpleWidths fi of
            Nothing -> 0
            Just (firstChar, lastChar, widths) ->
              if code >= firstChar && code <= lastChar && (code - firstChar) < length widths
                 then (widths !! (code - firstChar)) / 1000
                 else 0
    in (Glyph {
      glyphCode = code,
      glyphTopLeft = Vector 0 0,
      glyphBottomRight = Vector width 1,
      glyphText = txt
      }, width)
fontInfoDecodeGlyphs (FontInfoComposite fi) = \str ->
  case fiCompositeUnicodeCMap fi of
    Nothing -> []  -- XXX: use encoding here
    Just toUnicode ->
      let getWidth = fromMaybe (fiCompositeDefaultWidth fi) . cidFontGetWidth (fiCompositeWidths fi)
      in cmapDecodeString getWidth toUnicode str

cmapDecodeString :: (Int -> Double) -> UnicodeCMap -> Str -> [(Glyph, Double)]
cmapDecodeString getWidth cmap (Str str) = go str
  where
  go s =
    case unicodeCMapNextGlyph cmap s of
      Nothing -> []
      Just (g, rest) ->
        let width = getWidth g / 1000
            glyph = Glyph {
          glyphCode = g,
          glyphTopLeft = Vector 0 0,
          glyphBottomRight = Vector width 1,
          glyphText = unicodeCMapDecodeGlyph cmap g
          }
        in (glyph, width) : go rest
