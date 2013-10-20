{-# LANGUAGE OverloadedStrings #-}

-- | Font dictionary

module Pdf.Toolbox.Document.FontDict
(
  FontDict,
  FontSubtype(..),
  fontDictSubtype,
  fontDictLoadInfo
)
where

import Data.Monoid
import Data.Functor
import Control.Monad
import qualified System.IO.Streams as Streams

import Pdf.Toolbox.Core
import Pdf.Toolbox.Content

import Pdf.Toolbox.Document.Internal.Types
import Pdf.Toolbox.Document.Monad

-- | Font subtypes
data FontSubtype
  = FontType0
  | FontType1
  | FontMMType1
  | FontType3
  | FontTrueType
  deriving (Show, Eq)

-- | Get font subtype
fontDictSubtype :: Monad m => FontDict -> PdfE m FontSubtype
fontDictSubtype (FontDict dict) = do
  Name str <- lookupDict "Subtype" dict >>= fromObject
  case str of
    "Type0" -> return FontType0
    "Type1" -> return FontType1
    "MMType1" -> return FontMMType1
    "Type3" -> return FontType3
    "TrueType" -> return FontTrueType
    _ -> left $ UnexpectedError $ "Unexpected font subtype: " ++ show str

-- | Load font info for the font
fontDictLoadInfo :: (MonadPdf m, MonadIO m) => FontDict -> PdfE m FontInfo
fontDictLoadInfo fd@(FontDict fontDict) = do
  subtype <- fontDictSubtype fd
  case subtype of
    FontType0 -> FontInfoComposite <$> loadFontInfoComposite fontDict
    _ -> FontInfoSimple <$> loadFontInfoSimple fontDict

loadFontInfoComposite :: (MonadPdf m, MonadIO m) => Dict -> PdfE m FIComposite
loadFontInfoComposite fontDict = do
  toUnicode <- loadUnicodeCMap fontDict
  descFont <- do
    descFontArr <- lookupDict "DescendantFonts" fontDict >>= deref >>= fromObject
    case descFontArr of
      Array [o] -> deref o >>= fromObject
      _ -> left $ UnexpectedError "Unexpected value of DescendantFonts key in font dictionary"
  defaultWidth <-
    case lookupDict' "DW" descFont of
      Nothing -> return 1000
      Just o -> deref o >>= fromObject >>= realValue
  widths <-
    case lookupDict' "W" descFont of
      Nothing -> return mempty
      Just o -> deref o >>= fromObject >>= makeCIDFontWidths
  return $ FIComposite {
    fiCompositeUnicodeCMap = toUnicode,
    fiCompositeWidths = widths,
    fiCompositeDefaultWidth = defaultWidth
    }

loadFontInfoSimple :: (MonadPdf m, MonadIO m) => Dict -> PdfE m FISimple
loadFontInfoSimple fontDict = do
  toUnicode <- loadUnicodeCMap fontDict
  encoding <-
    case lookupDict' "Encoding" fontDict of
      Just (OName "WinAnsiEncoding") -> return $ Just SimpleFontEncodingWinAnsi
      Just (OName "MacRomanEncoding") -> return $ Just SimpleFontEncodingMacRoman
      _ -> return Nothing
  widths <-
    case lookupDict' "Widths" fontDict of
      Nothing -> return Nothing
      Just v -> do
        Array array <- deref v >>= fromObject
        widths <- mapM (fromObject >=> realValue) array
        firstChar <- lookupDict "FirstChar" fontDict >>= fromObject >>= intValue
        lastChar <- lookupDict "LastChar" fontDict >>= fromObject >>= intValue
        return $ Just (firstChar, lastChar, widths)
  return $ FISimple {
    fiSimpleUnicodeCMap = toUnicode,
    fiSimpleEncoding = encoding,
    fiSimpleWidths = widths
    }

loadUnicodeCMap :: (MonadPdf m, MonadIO m) => Dict -> PdfE m (Maybe UnicodeCMap)
loadUnicodeCMap fontDict =
  case lookupDict' "ToUnicode" fontDict of
    Nothing -> return Nothing
    Just o -> do
      ref <- fromObject o
      toUnicode <- lookupObject ref
      case toUnicode of
        OStream s -> do
          Stream _ is <- streamContent ref s
          content <- mconcat <$> liftIO (Streams.toList is)
          case parseUnicodeCMap content of
            Left e -> left $ UnexpectedError $ "can't parse cmap: " ++ show e
            Right cmap -> return $ Just cmap
        _ -> left $ UnexpectedError "ToUnicode: not a stream"
