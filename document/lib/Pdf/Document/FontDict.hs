{-# LANGUAGE OverloadedStrings #-}

-- | Font dictionary

module Pdf.Document.FontDict
(
  FontDict,
  FontSubtype(..),
  fontDictSubtype,
  fontDictLoadInfo,
)
where

import Pdf.Core.Object
import Pdf.Core.Object.Util
import Pdf.Core.Exception
import Pdf.Core.Util
import Pdf.Core.Types
import qualified Pdf.Core.Name as Name
import Pdf.Content

import Pdf.Document.Pdf
import Pdf.Document.Internal.Types

import Data.Word
import Data.ByteString (ByteString)
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Exception hiding (throw)
import qualified System.IO.Streams as Streams
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (ignore)
import qualified Data.Text as Text

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
  obj <- sure (HashMap.lookup "Subtype" dict
              `notice` "Subtype should exist")
            >>= deref pdf
  str <- sure $ nameValue obj `notice` "Subtype should be a name"
  case str of
    "Type0" -> return FontType0
    "Type1" -> return FontType1
    "MMType1" -> return FontMMType1
    "Type3" -> return FontType3
    "TrueType" -> return FontTrueType
    _ -> throwIO $ Unexpected ("Unexpected font subtype: " ++ show str) []

-- | Load font info for the font
fontDictLoadInfo :: FontDict -> IO FontInfo
fontDictLoadInfo fd@(FontDict pdf fontDict) = do
  subtype <- fontDictSubtype fd
  case subtype of
    FontType0 -> FontInfoComposite <$> loadFontInfoComposite pdf fontDict
    FontType3 -> do
      fi <- loadFontInfoSimple pdf fontDict
      obj <- sure (HashMap.lookup "FontMatrix" fontDict
                    `notice` "FontMatrix should exist")
              >>= deref pdf
      arr <- sure $ arrayValue obj
                    `notice` "FontMatrix should be an array"
      fontMatrix <-
        case mapM realValue (Vector.toList arr) of
          Just [a, b, c, d, e, f] -> do
            return $ Transform a b c d e f
          Nothing -> throwIO $ Corrupted "FontMatrics should contain numbers" []
          _ -> throwIO $ Corrupted "FontMatrix: wrong number of elements" []
      return $ FontInfoSimple fi {
        fiSimpleFontMatrix = fontMatrix
        }
    _ -> FontInfoSimple <$> loadFontInfoSimple pdf fontDict

loadFontInfoComposite :: Pdf -> Dict -> IO FIComposite
loadFontInfoComposite pdf fontDict = do
  toUnicode <- loadUnicodeCMap pdf fontDict

  descFont <- do
    descFontObj <- sure (HashMap.lookup "DescendantFonts" fontDict
                          `notice` "DescendantFonts should exist")
                    >>= deref pdf
    descFontArr <- sure $ arrayValue descFontObj
        `notice` "DescendantFonts should be an array"
    case Vector.toList descFontArr of
      [o] -> do
        o' <- deref pdf o
        sure $ dictValue o'
                `notice` "DescendantFonts element should be a dictionary"
      _ -> throwIO $ Corrupted
            "Unexpected value of DescendantFonts key in font dictionary" []

  defaultWidth <-
    case HashMap.lookup "DW" descFont of
      Nothing -> return 1000
      Just o -> do
        o' <- deref pdf o
        sure $ realValue o' `notice` "DW should be real"

  widths <-
    case HashMap.lookup "W" descFont of
      Nothing -> return mempty
      Just o -> do
        o' <- deref pdf o
        arr <- sure (arrayValue o' `notice` "W should be an array")
          >>= Vector.mapM (deref pdf)
        sure $ makeCIDFontWidths arr

  fontDescriptor <- loadFontDescriptor pdf descFont

  return $ FIComposite {
    fiCompositeUnicodeCMap = toUnicode,
    fiCompositeWidths = widths,
    fiCompositeDefaultWidth = defaultWidth,
    fiCompositeFontDescriptor = fontDescriptor
    }

loadFontInfoSimple :: Pdf -> Dict -> IO FISimple
loadFontInfoSimple pdf fontDict = do
  toUnicode <- loadUnicodeCMap pdf fontDict

  encoding <-
    case HashMap.lookup "Encoding" fontDict of
      Just (Name "WinAnsiEncoding") -> return $ Just SimpleFontEncoding
        { simpleFontBaseEncoding = FontBaseEncodingWinAnsi
        , simpleFontDifferences = []
        }
      Just (Name "MacRomanEncoding") -> return $ Just SimpleFontEncoding
        { simpleFontBaseEncoding = FontBaseEncodingMacRoman
        , simpleFontDifferences = []
        }
      Just o -> do
        o' <- deref pdf o
        encDict <- sure (dictValue o'
                      `notice` "Encoding should be a dictionary")
        case HashMap.lookup "BaseEncoding" encDict of
          Just (Name "WinAnsiEncoding") -> do
            diffs <- loadEncodingDifferences pdf encDict
            return $ Just SimpleFontEncoding
              { simpleFontBaseEncoding = FontBaseEncodingWinAnsi
              , simpleFontDifferences = diffs
              }
          Just (Name "MacRomanEncoding") -> do
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
    case HashMap.lookup "Widths" fontDict of
      Nothing -> return Nothing
      Just v -> do
        v' <- deref pdf v
        array <- sure $ arrayValue v'
            `notice` "Widths should be an array"
        widths <- forM (Vector.toList array) $ \o ->
          sure (realValue o `notice` "Widths elements should be real")
        firstChar <- sure $ (HashMap.lookup "FirstChar" fontDict >>= intValue)
                `notice` "FirstChar should be an integer"
        lastChar <- sure $ (HashMap.lookup "LastChar" fontDict >>= intValue)
                `notice` "LastChar should be an integer"
        return $ Just (firstChar, lastChar, widths)

  fontDescriptor <- loadFontDescriptor pdf fontDict

  return $ FISimple
    { fiSimpleUnicodeCMap = toUnicode
    , fiSimpleEncoding = encoding
    , fiSimpleWidths = widths
    , fiSimpleFontMatrix = scale 0.001 0.001
    , fiSimpleFontDescriptor = fontDescriptor
    }

loadEncodingDifferences :: Pdf -> Dict -> IO [(Word8, ByteString)]
loadEncodingDifferences pdf dict = do
  case HashMap.lookup "Differences" dict of
    Nothing -> return []
    Just v -> do
      v' <- deref pdf v
      arr <- sure $ arrayValue v'
          `notice` "Differences should be an array"
      case Vector.toList arr of
        [] -> return []
        (o : rest) -> do
          n' <- fromIntegral <$> (sure $ intValue o
                  `notice` "Differences: the first element should be integer")
          go [] n' rest
  where
  go res _ [] = return res
  go res n (o:rest) =
    case o of
      (Number _) -> do
        n' <- fromIntegral <$> (sure $ intValue o
          `notice` "Differences: elements should be integers")
        go res n' rest
      (Name name) -> go (((n, Name.toByteString name)) : res) (n + 1) rest
      _ -> throwIO $ Corrupted
        ("Differences array: unexpected object: " ++ show o) []

loadUnicodeCMap :: Pdf -> Dict -> IO (Maybe UnicodeCMap)
loadUnicodeCMap pdf fontDict =
  case HashMap.lookup "ToUnicode" fontDict of
    Nothing -> return Nothing
    Just o -> do
      ref <- sure $ refValue o
        `notice` "ToUnicode should be a reference"
      toUnicode <- lookupObject pdf ref
      case toUnicode of
        Stream s -> do
          is <- streamContent pdf ref s
          content <- mconcat <$> Streams.toList is
          case parseUnicodeCMap content of
            Left e -> throwIO $ Corrupted ("can't parse cmap: " ++ show e) []
            Right cmap -> return $ Just cmap
        _ -> throwIO $ Corrupted "ToUnicode: not a stream" []


loadFontDescriptor :: Pdf -> Dict -> IO (Maybe FontDescriptor)
loadFontDescriptor pdf fontDict = do
  case HashMap.lookup "FontDescriptor" fontDict of
    Nothing -> return Nothing
    Just o -> do
      ref <- sure $ refValue o
             `notice` "FontDescriptor should be a reference"
      fd <- (sure . (`notice` "FontDescriptor: not a dictionary") . dictValue) =<<
            lookupObject pdf ref

      fontName <- required "FontName" nameValue' fd
      fontFamily <- optional "FontFamily" stringValue fd
      fontStretch <- optional "FontStretch" nameValue' fd
      fontWeight <- optional "FontWeight" intValue fd
      flags <- required "Flags" int64Value fd
      fontBBox <- optional "FontBBox"
        (join . fmap (either (const Nothing) Just . rectangleFromArray) . arrayValue) fd
      italicAngle <- required "ItalicAngle" realValue fd
      ascent <- optional "Ascent" realValue fd
      descent <- optional "Descent" realValue fd
      leading <- optional "Leading" realValue fd
      capHeight <- optional "CapHeight" realValue fd
      xHeight <- optional "XHeight" realValue fd
      stemV <- optional "StemV" realValue fd
      stemH <- optional "StemH" realValue fd
      avgWidth <- optional "AvgWidth" realValue fd
      maxWidth <- optional "MaxWidth" realValue fd
      missingWidth <- optional "MissingWidth" realValue fd
      charSet <- optional "CharSet" stringValue fd

      return $ Just $ FontDescriptor
        { fdFontName = fontName
        , fdFontFamily = fontFamily
        , fdFontStretch = fontStretch
        , fdFontWeight = fontWeight
        , fdFlags = flags
        , fdFontBBox = fontBBox
        , fdItalicAngle = italicAngle
        , fdDescent = descent
        , fdAscent = ascent
        , fdLeading = leading
        , fdCapHeight = capHeight
        , fdXHeight = xHeight
        , fdStemV = stemV
        , fdStemH = stemH
        , fdAvgWidth = avgWidth
        , fdMaxWidth = maxWidth
        , fdMissingWidth = missingWidth
        , fdCharSet = charSet
        }
  where
    required = requiredInDict "FontDescriptor"
    optional = optionalInDict "FontDescriptor"
    nameValue' = fmap Name.toByteString . nameValue

-- | Parse a value from a required field of a dictionary. This will
-- raise an exception if a) the field is not present or b) the field
-- value has a false type.
requiredInDict :: String -- ^ a context for a failure notice
               -> Name   -- ^ name of dictionary field
               -> (Object -> Maybe a) -- ^ function for type-casting the object
               -> Dict                -- ^ the dictionary
               -> IO a
requiredInDict context key typeFun =
  join .
  sure . (`notice` (context ++ ": " ++ msg ++ " should exist")) .
  fmap (sure . (`notice` (context ++ ": " ++ msg ++ " type failure")) . typeFun) .
  HashMap.lookup key
  where
    msg = Text.unpack $ decodeUtf8With ignore $ Name.toByteString key

-- | Parse a value from an optional field of a dictionary. This will
-- raise an exception if the field value has a false type.
optionalInDict :: String -> Name -> (Object -> Maybe a) -> Dict -> IO (Maybe a)
optionalInDict context key typeFun =
  maybe (return Nothing) (liftM Just) .
  fmap (sure . (`notice` (context ++ ": " ++ msg ++ " type failure")) . typeFun) .
  HashMap.lookup key
  where
    msg = Text.unpack $ decodeUtf8With ignore $ Name.toByteString key
