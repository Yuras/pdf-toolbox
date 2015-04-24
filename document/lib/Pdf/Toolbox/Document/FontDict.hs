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

import Data.Word
import Data.ByteString (ByteString)
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
    FontType3 -> do
      fi <- loadFontInfoSimple fontDict
      Array arr <- lookupDict "FontMatrix" fontDict >>= deref >>= fromObject
      fontMatrix <-
        case arr of
          [a, b, c, d, e, f] -> do
            a' <- fromObject a >>= realValue
            b' <- fromObject b >>= realValue
            c' <- fromObject c >>= realValue
            d' <- fromObject d >>= realValue
            e' <- fromObject e >>= realValue
            f' <- fromObject f >>= realValue
            return $ Transform a' b' c' d' e' f'
          _ -> left $ UnexpectedError "FontMatrix: wrong number of elements"
      return $ FontInfoSimple fi {
        fiSimpleFontMatrix = fontMatrix
        }
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
      Just (OName "WinAnsiEncoding") -> return $ Just SimpleFontEncoding {
        simpleFontBaseEncoding = FontBaseEncodingWinAnsi,
        simpleFontDifferences = []
        }
      Just (OName "MacRomanEncoding") -> return $ Just SimpleFontEncoding {
        simpleFontBaseEncoding = FontBaseEncodingMacRoman,
        simpleFontDifferences = []
        }
      Just o -> do
        encDict <- deref o >>= fromObject
        case lookupDict' "BaseEncoding" encDict of
          Just (OName "WinAnsiEncoding") -> do
            diffs <- loadEncodingDifferences encDict
            return $ Just SimpleFontEncoding {
              simpleFontBaseEncoding = FontBaseEncodingWinAnsi,
              simpleFontDifferences = diffs
              }
          Just (OName "MacRomanEncoding") -> do
            diffs <- loadEncodingDifferences encDict
            return $ Just SimpleFontEncoding {
              simpleFontBaseEncoding = FontBaseEncodingMacRoman,
              simpleFontDifferences = diffs
              }
          Nothing -> do
            diffs <- loadEncodingDifferences encDict
            return $ Just SimpleFontEncoding {
              simpleFontBaseEncoding = FontBaseEncodingWinAnsi,  -- XXX: should be StandardEncoding?
              simpleFontDifferences = diffs
              }
          _ -> return Nothing
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
    fiSimpleWidths = widths,
    fiSimpleFontMatrix = scale 0.001 0.001
    }

loadEncodingDifferences :: MonadPdf m => Dict -> PdfE m [(Word8, ByteString)]
loadEncodingDifferences dict = do
  case lookupDict' "Differences" dict of
    Nothing -> return []
    Just o -> do
      Array arr <- deref o >>= fromObject
      case arr of
        [] -> return []
        (ONumber n : rest) -> do
          n' <- fromIntegral <$> intValue n
          go [] n' rest
        _ -> left $ UnexpectedError "Differences array: the first object should be a number"
  where
  go res _ [] = return res
  go res n (o:rest) =
    case o of
      (ONumber n') -> do
        n'' <- fromIntegral <$> intValue n'
        go res n'' rest
      (OName (Name bs)) -> go (((n, bs)) : res) (n + 1) rest
      _ -> left $ UnexpectedError $ "Differences array: unexpected object: " ++ show o

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
