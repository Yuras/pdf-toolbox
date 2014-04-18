
-- | Font info contains information, extracted from font,
-- that may be needed when processing content stream

module Pdf.Toolbox.Content.FontInfo
(
  FontInfo(..),
  FISimple(..),
  FontBaseEncoding(..),
  SimpleFontEncoding(..),
  FIComposite(..),
  CIDFontWidths(..),
  makeCIDFontWidths,
  cidFontGetWidth,
  fontInfoDecodeGlyphs
)
where

import Data.List
import Data.Word
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Control.Monad

import Pdf.Toolbox.Core

import Pdf.Toolbox.Content.UnicodeCMap
import Pdf.Toolbox.Content.Transform
import Pdf.Toolbox.Content.Processor (Glyph(..))
import Pdf.Toolbox.Content.GlyphList
import Pdf.Toolbox.Content.TexGlyphList
import qualified Pdf.Toolbox.Content.Encoding.WinAnsi as WinAnsi
import qualified Pdf.Toolbox.Content.Encoding.MacRoman as MacRoman

-- | Font info
data FontInfo
  = FontInfoSimple FISimple
  | FontInfoComposite FIComposite
  deriving (Show)

-- | Font info for simple fonts
data FISimple = FISimple {
  fiSimpleUnicodeCMap :: Maybe UnicodeCMap,
  fiSimpleEncoding :: Maybe SimpleFontEncoding,
  fiSimpleWidths :: Maybe (Int, Int, [Double]),  -- ^ FirstChar, LastChar, list of widths
  fiSimpleFontMatrix :: Transform Double
  }
  deriving (Show)

-- | Standard encoding, other encodings are based on them
data FontBaseEncoding
  = FontBaseEncodingWinAnsi
  | FontBaseEncodingMacRoman
  deriving (Show)

-- | Encoding fo simple font
data SimpleFontEncoding = SimpleFontEncoding {
  simpleFontBaseEncoding :: FontBaseEncoding,
  -- | Mapping from glyph code to glyph name for cases when it is different
  -- from base encoding
  simpleFontDifferences :: [(Word8, ByteString)]
  }
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

simpleFontEncodingDecode :: SimpleFontEncoding -> Word8 -> Maybe Text
simpleFontEncodingDecode enc code =
  case lookup code (simpleFontDifferences enc) of
    Nothing ->
      case simpleFontBaseEncoding enc of
        FontBaseEncodingWinAnsi -> Map.lookup code WinAnsi.encoding
        FontBaseEncodingMacRoman -> Map.lookup code MacRoman.encoding
    Just glyphName ->
      case Map.lookup glyphName adobeGlyphList of
        Just c -> Just $ Text.pack [c]
        Nothing ->
          case Map.lookup glyphName texGlyphList of
            Nothing-> Nothing
            Just c -> Just $ Text.pack [c]

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
                Just enc ->
                  case simpleFontEncodingDecode enc c of
                    Just t -> Just t
                    Nothing ->
                      case Text.decodeUtf8' (BS.pack [c]) of
                        Right t -> Just t
                        _ -> Nothing
            Just toUnicode ->
              case unicodeCMapDecodeGlyph toUnicode code of
                Just t -> Just t
                Nothing ->
                  case fiSimpleEncoding fi of
                    Nothing -> Nothing
                    Just enc ->
                      case simpleFontEncodingDecode enc c of
                        Just t -> Just t
                        Nothing ->
                          case Text.decodeUtf8' (BS.pack [c]) of
                            Right t -> Just t
                            _ -> Nothing
        width =
          case fiSimpleWidths fi of
            Nothing -> 0
            Just (firstChar, lastChar, widths) ->
              if code >= firstChar && code <= lastChar && (code - firstChar) < length widths
                 then let Vector w _ = transform (fiSimpleFontMatrix fi) $ Vector (widths !! (code - firstChar)) 0
                      in w
                 else 0
    in (Glyph {
      glyphCode = code,
      glyphTopLeft = Vector 0 0,
      glyphBottomRight = Vector width 1,
      glyphText = txt
      }, width)
fontInfoDecodeGlyphs (FontInfoComposite fi) = \str ->
  case fiCompositeUnicodeCMap fi of
    Nothing ->  -- XXX: use encoding here
      let Str bs = str
      in tryDecode2byte $ BS.unpack bs
    Just toUnicode ->
      let getWidth = fromMaybe (fiCompositeDefaultWidth fi) . cidFontGetWidth (fiCompositeWidths fi)
      in cmapDecodeString getWidth toUnicode str
  where
  -- Most of the time composite fonts have 2-byte encoding,
  -- so lets try that for now.
  tryDecode2byte (b1:b2:rest) =
    let code = fromIntegral b1 * 255 + fromIntegral b2
        width = (/ 1000) $ fromMaybe (fiCompositeDefaultWidth fi) $ cidFontGetWidth (fiCompositeWidths fi) code
        txt =
          case Text.decodeUtf8' (BS.pack [b1, b2]) of
            Right t -> Just t
            _ -> Nothing
        g = Glyph {
          glyphCode = code,
          glyphTopLeft = Vector 0 0,
          glyphBottomRight = Vector width 1,
          glyphText = txt
          }
    in (g, width) : tryDecode2byte rest
  tryDecode2byte _ = []

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
