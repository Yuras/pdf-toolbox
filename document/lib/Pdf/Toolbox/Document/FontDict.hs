{-# LANGUAGE OverloadedStrings #-}

-- | Font dictionary

module Pdf.Toolbox.Document.FontDict
(
  FontDict,
  FontSubtype(..),
  fontDictSubtype,
  fontDictLoadInfo,
)
where

import Data.Word
import Data.Monoid
import Data.Functor
import Data.ByteString (ByteString)
import Control.Monad
import Control.Exception
import qualified System.IO.Streams as Streams

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Util
import Pdf.Toolbox.Content

import Pdf.Toolbox.Document.Pdf
import Pdf.Toolbox.Document.Internal.Types

-- | Font subtypes
data FontSubtype
  = FontType0
  | FontType1
  | FontMMType1
  | FontType3
  | FontTrueType
  deriving (Show, Eq)

-- | Get font subtype
fontDictSubtype :: FontDict -> IO FontSubtype
fontDictSubtype (FontDict pdf dict) = do
  obj <- sure (lookupDict "Subtype" dict
              `notice` "Subtype should exist")
            >>= deref pdf
  Name str <- sure $ nameValue obj `notice` "Subtype should be a name"
  case str of
    "Type0" -> return FontType0
    "Type1" -> return FontType1
    "MMType1" -> return FontMMType1
    "Type3" -> return FontType3
    "TrueType" -> return FontTrueType
    _ -> throw $ Unexpected ("Unexpected font subtype: " ++ show str) []

-- | Load font info for the font
fontDictLoadInfo :: FontDict -> IO FontInfo
fontDictLoadInfo fd@(FontDict pdf fontDict) = do
  subtype <- fontDictSubtype fd
  case subtype of
    FontType0 -> FontInfoComposite <$> loadFontInfoComposite pdf fontDict
    FontType3 -> do
      fi <- loadFontInfoSimple pdf fontDict
      obj <- sure (lookupDict "FontMatrix" fontDict
                    `notice` "FontMatrix should exist")
              >>= deref pdf
      Array arr <- sure $ arrayValue obj
                    `notice` "FontMatrix should be an array"
      fontMatrix <-
        case mapM realValue arr of
          Just [a, b, c, d, e, f] -> do
            return $ Transform a b c d e f
          Nothing -> throw $ Corrupted "FontMatrics should contain numbers" []
          _ -> throw $ Corrupted "FontMatrix: wrong number of elements" []
      return $ FontInfoSimple fi {
        fiSimpleFontMatrix = fontMatrix
        }
    _ -> FontInfoSimple <$> loadFontInfoSimple pdf fontDict

loadFontInfoComposite :: Pdf -> Dict -> IO FIComposite
loadFontInfoComposite pdf fontDict = do
  toUnicode <- loadUnicodeCMap pdf fontDict

  descFont <- do
    descFontObj <- sure (lookupDict "DescendantFonts" fontDict
                          `notice` "DescendantFonts should exist")
                    >>= deref pdf
    descFontArr <- sure $ arrayValue descFontObj
        `notice` "DescendantFonts should be an array"
    case descFontArr of
      Array [o] -> do
        o' <- deref pdf o
        sure $ dictValue o'
                `notice` "DescendantFonts element should be a dictionary"
      _ -> throw $ Corrupted
            "Unexpected value of DescendantFonts key in font dictionary" []

  defaultWidth <-
    case lookupDict "DW" descFont of
      Nothing -> return 1000
      Just o -> do
        o' <- deref pdf o
        sure $ realValue o' `notice` "DW should be real"

  widths <-
    case lookupDict "W" descFont of
      Nothing -> return mempty
      Just o -> do
        o' <- deref pdf o
        arr <- sure (arrayValue o' `notice` "W should be an array")
        sure $ makeCIDFontWidths arr

  return $ FIComposite {
    fiCompositeUnicodeCMap = toUnicode,
    fiCompositeWidths = widths,
    fiCompositeDefaultWidth = defaultWidth
    }

loadFontInfoSimple :: Pdf -> Dict -> IO FISimple
loadFontInfoSimple pdf fontDict = do
  toUnicode <- loadUnicodeCMap pdf fontDict

  encoding <-
    case lookupDict "Encoding" fontDict of
      Just (OName "WinAnsiEncoding") -> return $ Just SimpleFontEncoding
        { simpleFontBaseEncoding = FontBaseEncodingWinAnsi
        , simpleFontDifferences = []
        }
      Just (OName "MacRomanEncoding") -> return $ Just SimpleFontEncoding
        { simpleFontBaseEncoding = FontBaseEncodingMacRoman
        , simpleFontDifferences = []
        }
      Just o -> do
        o' <- deref pdf o
        encDict <- sure (dictValue o'
                      `notice` "Encoding should be a dictionary")
        case lookupDict "BaseEncoding" encDict of
          Just (OName "WinAnsiEncoding") -> do
            diffs <- loadEncodingDifferences pdf encDict
            return $ Just SimpleFontEncoding
              { simpleFontBaseEncoding = FontBaseEncodingWinAnsi
              , simpleFontDifferences = diffs
              }
          Just (OName "MacRomanEncoding") -> do
            diffs <- loadEncodingDifferences pdf encDict
            return $ Just SimpleFontEncoding
              { simpleFontBaseEncoding = FontBaseEncodingMacRoman
              , simpleFontDifferences = diffs
              }
          Nothing -> do
            diffs <- loadEncodingDifferences pdf encDict
            return $ Just SimpleFontEncoding
              -- XXX: should be StandardEncoding?
              { simpleFontBaseEncoding = FontBaseEncodingWinAnsi
              , simpleFontDifferences = diffs
              }
          _ -> return Nothing
      _ -> return Nothing

  widths <-
    case lookupDict "Widths" fontDict of
      Nothing -> return Nothing
      Just v -> do
        v' <- deref pdf v
        Array array <- sure $ arrayValue v'
            `notice` "Widths should be an array"
        widths <- forM array $ \o ->
          sure (realValue o `notice` "Widths elements should be real")
        firstChar <- sure $ (lookupDict "FirstChar" fontDict >>= intValue)
                `notice` "FirstChar should be an integer"
        lastChar <- sure $ (lookupDict "LastChar" fontDict >>= intValue)
                `notice` "LastChar should be an integer"
        return $ Just (firstChar, lastChar, widths)

  return $ FISimple
    { fiSimpleUnicodeCMap = toUnicode
    , fiSimpleEncoding = encoding
    , fiSimpleWidths = widths
    , fiSimpleFontMatrix = scale 0.001 0.001
    }

loadEncodingDifferences :: Pdf -> Dict -> IO [(Word8, ByteString)]
loadEncodingDifferences pdf dict = do
  case lookupDict "Differences" dict of
    Nothing -> return []
    Just v -> do
      v' <- deref pdf v
      Array arr <- sure $ arrayValue v'
          `notice` "Differences should be an array"
      case arr of
        [] -> return []
        (o : rest) -> do
          n' <- fromIntegral <$> (sure $ intValue o
                  `notice` "Differences: the first element should be integer")
          go [] n' rest
  where
  go res _ [] = return res
  go res n (o:rest) =
    case o of
      (ONumber _) -> do
        n' <- fromIntegral <$> (sure $ intValue o
          `notice` "Differences: elements should be integers")
        go res n' rest
      (OName (Name bs)) -> go (((n, bs)) : res) (n + 1) rest
      _ -> throw $ Corrupted
        ("Differences array: unexpected object: " ++ show o) []

loadUnicodeCMap :: Pdf -> Dict -> IO (Maybe UnicodeCMap)
loadUnicodeCMap pdf fontDict =
  case lookupDict "ToUnicode" fontDict of
    Nothing -> return Nothing
    Just o -> do
      o' <- deref pdf o
      ref <- sure $refValue o'
        `notice` "ToUnicode should be a reference"
      toUnicode <- lookupObject pdf ref
      case toUnicode of
        OStream s -> do
          Stream _ is <- streamContent pdf ref s
          content <- mconcat <$> Streams.toList is
          case parseUnicodeCMap content of
            Left e -> throw $ Corrupted ("can't parse cmap: " ++ show e) []
            Right cmap -> return $ Just cmap
        _ -> throw $ Corrupted "ToUnicode: not a stream" []
