
-- | The FontDescriptor describes a font's other metrics than it's
-- widths, see chap. 9.8 of PDF32000:2008

module Pdf.Content.FontDescriptor
  ( FontDescriptor(..)
  , FontDescriptorFlag(..)
  , flagSet
  )
where

import Pdf.Core.Types

import Data.Int
import Data.ByteString (ByteString)

data FontDescriptor = FontDescriptor {
  fdFontName :: ByteString,
  fdFontFamily :: Maybe ByteString,
  fdFontStretch :: Maybe ByteString,
  fdFontWeight :: Maybe Int,
  fdFlags :: Int64, -- must hold at least 32 bit unsigned integers
  fdFontBBox :: Maybe (Rectangle Double),
  fdItalicAngle :: Double,
  fdAscent :: Maybe Double,
  fdDescent :: Maybe Double,
  fdLeading :: Maybe Double,
  fdCapHeight :: Maybe Double,
  fdXHeight :: Maybe Double,
  fdStemV :: Maybe Double,
  fdStemH :: Maybe Double,
  fdAvgWidth :: Maybe Double,
  fdMaxWidth :: Maybe Double,
  fdMissingWidth :: Maybe Double,
  -- FIXME: add FontFile*
  fdCharSet :: Maybe ByteString
  -- FIXME: add special fields for CIDFonts
  } deriving (Show)


data FontDescriptorFlag =
  FixedPitch | Serif | Symbolic | Script | NonSymbolic | Italic | AllCap | SmallCap | ForceBold


flagSet :: FontDescriptor -> FontDescriptorFlag -> Bool
flagSet fd FixedPitch = flagSet' 1 (fdFlags fd) 0
flagSet fd Serif = flagSet' 2 (fdFlags fd) 0
flagSet fd Symbolic = flagSet' 3 (fdFlags fd) 0
flagSet fd Script = flagSet' 4 (fdFlags fd) 0
flagSet fd NonSymbolic = flagSet' 6 (fdFlags fd) 0
flagSet fd Italic = flagSet' 7 (fdFlags fd) 0
flagSet fd AllCap = flagSet' 17 (fdFlags fd) 0
flagSet fd SmallCap = flagSet' 18 (fdFlags fd) 0
flagSet fd ForceBold = flagSet' 19 (fdFlags fd) 0

flagSet' :: Int -> Int64 -> Int -> Bool
flagSet' pos val expnt
  | expnt == pos - 1 = val `mod` 2 == 1
  | val == 0 = False
  | otherwise = flagSet' pos (val `div` 2) (expnt+1)
