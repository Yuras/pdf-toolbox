{-# LANGUAGE OverloadedStrings #-}

-- | Unicode CMap defines mapping from glyphs to text.
--
-- Technical specs can be found here:
-- (1) https://www.adobe.com/content/dam/acom/en/devnet/font/pdfs/5099.CMapResources.pdf
-- and p. 48 seqq of
-- (2) https://www.adobe.com/content/dam/acom/en/devnet/font/pdfs/5014.CIDFont_Spec.pdf
-- see also:
-- (3) https://github.com/adobe-type-tools/cmap-resources


module Pdf.Content.UnicodeCMap
(
  UnicodeCMap(..),
  parseUnicodeCMap,
  unicodeCMapNextGlyph,
  unicodeCMapDecodeGlyphWith,
  unicodeCMapDecodeGlyph,
  toCode
)
where

import Data.Char
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base16 as Base16
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Data.Word (Word8)

-- | Unicode character map
--
-- Font dictionary can contain \"ToUnicode\" key -- reference
-- to a stream with unicode CMap
data UnicodeCMap = UnicodeCMap {
  unicodeCMapCodeRanges :: [(ByteString, ByteString)],
  unicodeCMapChars :: Map Int Text,
  unicodeCMapRanges :: [(Int, Int, Char)]
  }
  deriving (Show)

-- | Parse content of unicode CMap
parseUnicodeCMap :: ByteString -> Either String UnicodeCMap
parseUnicodeCMap cmap =
  case (codeRanges, chars, ranges) of
    (Right cr, Right cs, Right (rs, crs)) -> Right $ UnicodeCMap {
      unicodeCMapCodeRanges = cr,
      unicodeCMapChars = cs <> crs,
      unicodeCMapRanges = rs
      }
    (Left err, _, _) -> Left $ "CMap code ranges: " ++ err
    (_, Left err, _) -> Left $ "CMap chars: " ++ err
    (_, _, Left err) -> Left $ "CMap ranges: " ++ err
  where
  codeRanges = parseOnly codeRangesParser cmap
  chars = parseOnly charsParser cmap
  ranges = parseOnly rangesParser cmap

-- | Return a pair of functions: One that takes the next glyph code
-- from string and also returns the rest of the string, and a second
-- that decodes a byte string to text.
unicodeCMapNextGlyph :: UnicodeCMap -> ((ByteString -> Maybe (ByteString, ByteString)),
                                        (ByteString -> Maybe Text))
unicodeCMapNextGlyph (UnicodeCMap rs@[("\x00\x00","\xFF\xFF")] _ _) =
  -- UCS-2, according to (1)
  ((fixedWidthMultiByte 2 rs), (Just . decodeUcs2))
unicodeCMapNextGlyph (UnicodeCMap rs@[ ("\x00\x00","\xD7\xFF")
                                     , ("\xE0\x00","\xFF\xFF")] _ _) =
  -- UCS-2, according to (2)
  ((fixedWidthMultiByte 2 rs), (Just . decodeUcs2))
unicodeCMapNextGlyph (UnicodeCMap rs@[("\x00\x00\x00\x00","\x00\x10\xFF\xFF")] _ _) =
  -- UTF32 (UTF32-BE)
  ((fixedWidthMultiByte 4 rs), (Just . Text.decodeUtf32BE))
unicodeCMapNextGlyph (UnicodeCMap rs@[ ("\x00\x00","\xD7\FF")
                                     , ("\xE0\x00","\xFF\xFF")
                                     , ("\xD8\x00\xDC\x00", "\xDB\xFF\xDF\xFF")] _ _) =
  -- UTF16 (UTF16-BE)
  ((variableWidthMultiByte 2 2 6 rs), (Just . Text.decodeUtf16BE))
unicodeCMapNextGlyph (UnicodeCMap rs@[ ("\x00","\xFF")] _ _) =
  -- 8 Bit encodings (assuming Latin1)
  ((fixedWidthMultiByte 1 rs), (Just . Text.decodeLatin1))
-- -- TODO: add more encodings: ISO 2022, EUC-TW, Big Five, see (3)
unicodeCMapNextGlyph (UnicodeCMap rs _ _) =
  -- Default: This works for UTF8 and 7 Bit encodings.
  ((variableWidthMultiByte 1 1 5 rs), ((either (const Nothing) Just) . Text.decodeUtf8'))


fixedWidthMultiByte :: Int -> [(ByteString, ByteString)] -> ByteString -> Maybe (ByteString, ByteString)
fixedWidthMultiByte l ranges bs =
  if ByteString.length bs >= l && inCMapRanges ranges bs
  then Just (ByteString.take l bs, ByteString.drop l bs)
  else Nothing

variableWidthMultiByte :: Int   -- ^ length in bytes to start with
                       -> Int   -- ^ length delta between steps
                       -> Int   -- ^ length at which to stop eating bytes
                       -> [(ByteString, ByteString)] -- ^ code space ranges
                       -> ByteString                 -- ^ string to eat and decode
                       -> Maybe (ByteString, ByteString)
variableWidthMultiByte startLength step stopLength ranges = go startLength
  where
    go n str =
      let glyph = ByteString.take n str in
        if ByteString.length glyph /= n || n >= stopLength
        then Nothing
        else if inCMapRanges ranges glyph
             then Just (glyph, ByteString.drop n str)
             else go (n + step) str

inCMapRanges :: [(ByteString, ByteString)] -> ByteString -> Bool
inCMapRanges ranges bs = any (inRange bs) ranges
  where
    inRange bs' (start, end) = bs' >= start && bs' <= end

toCode :: ByteString -> Int
toCode bs = fst $ ByteString.foldr (\b (sm, i) ->
                    (sm + fromIntegral b * i, i * 256)) (0, 1) bs

intToByteString :: Int -> ByteString
intToByteString = ByteString.pack . reverse . intToByteString'
  where
    intToByteString' :: Int -> [Word8]
    intToByteString' i
      | i `div` 256 == 0 = (fromIntegral $ i `mod` 256):[]
      | otherwise = (fromIntegral $ i `mod` 256):(intToByteString' (i `div` 256))

-- | Decode a *single character* represented as a 'ByteString' to
-- 'Text' mapping the full character range from 0x0000 to 0xFFFF
-- (different from 'Text.decodeUtf16BE'). Big endian is assumed. This
-- decoding function works for UCS-2. It even works for single byte
-- encodings, where it equals to ISO8859-1 (Latin1). It would even
-- work for UCS-4.
decodeUcs2 :: ByteString -> Text
decodeUcs2 = Text.singleton . toEnum . toCode

-- | Convert glyph to text
--
-- Note: one glyph can represent more then one char, e.g. for ligatures
unicodeCMapDecodeGlyphWith :: UnicodeCMap -> (ByteString -> Maybe Text) -> ByteString -> Maybe Text
unicodeCMapDecodeGlyphWith cmap decodeFun glyph =
  case Map.lookup glyphInt (unicodeCMapChars cmap) of
    Just txt -> Just txt
    Nothing ->
      case filter inRange (unicodeCMapRanges cmap) of
        [(0, _, '\x00')] -> decodeFun glyph
        [(start, _, char)] -> decodeFun $ intToByteString $ (fromEnum char) + (glyphInt - start)
        _ -> Nothing
  where
  glyphInt = toCode glyph
  inRange (start, end, _) = glyphInt >= start && glyphInt <= end

unicodeCMapDecodeGlyph :: UnicodeCMap -> Int -> Maybe Text
unicodeCMapDecodeGlyph cmap =
  (unicodeCMapDecodeGlyphWith cmap (Just . decodeUcs2)) . intToByteString

charsParser :: Parser (Map Int Text)
charsParser =
  combineChars <$> P.many' charsParser'
  where
  combineChars = List.foldl' Map.union Map.empty

charsParser' :: Parser (Map Int Text)
charsParser' = do
  n <- skipTillParser $ do
    n <- P.decimal
    P.skipSpace
    _ <- P.string "beginbfchar"
    return n

  chars <- replicateM n $ do
    P.skipSpace
    i <- parseHex
    P.skipSpace
    j <- parseHex
    return (toCode i, Text.decodeUtf16BE j)

  return $ Map.fromList chars

-- | It returns regular ranges and char map
--
-- Array ranges are converted to char map
rangesParser :: Parser ([(Int, Int, Char)], Map Int Text)
rangesParser =
  combineRanges <$> P.many' rangesParser'
  where
  combineRanges = List.foldl' combineRange ([], Map.empty)
  combineRange (ranges, rmap) (ranges', rmap') =
    (ranges ++ ranges', Map.union rmap rmap')

rangesParser' :: Parser ([(Int, Int, Char)], Map Int Text)
rangesParser' = do
  n <- skipTillParser $ do
    n <- P.decimal
    P.skipSpace
    void $ P.string "beginbfrange"
    return (n :: Int)

  let go 0 rs cs = return (rs, cs)
      go count rs cs = do
        P.skipSpace
        i <- toCode <$> parseHex
        P.skipSpace
        j <- toCode <$> parseHex
        P.skipSpace
        k <- P.eitherP parseHex parseHexArray
        case k of
          Left h -> do
            c <- case Text.uncons $ Text.decodeUtf16BE h of
                   Nothing -> fail "Can't decode range"
                   Just (v, _) -> return v
            go (pred count) ((i, j, c) : rs) cs
          Right hs -> do
            let cs' = zip [i..j] . map Text.decodeUtf16BE $ hs
            go (pred count) rs (cs <> Map.fromList cs')

  go n mempty mempty

codeRangesParser :: Parser [(ByteString, ByteString)]
codeRangesParser = do
  n <- skipTillParser $ do
    n <- P.decimal
    P.skipSpace
    void $ P.string "begincodespacerange"
    return n

  -- FIXME: in the specs this is limited to 100 in order to avoid stack overflows
  replicateM n $ do
    P.skipSpace
    i <- parseHex
    P.skipSpace
    j <- parseHex
    return (i, j)

parseHex :: Parser ByteString
parseHex = do
  void $ P.char '<'
  -- hex can contain spaces, lets filter them out
  res <- P.takeTill (== '>') >>= fromHex . ByteString.filter (/= 32)
  void $ P.char '>'
  return res

parseHexArray :: Parser [ByteString]
parseHexArray = do
  void $ P.char '['
  res <- P.many' $ do
    P.skipSpace
    parseHex
  P.skipSpace
  void $ P.char ']'
  return res

-- XXX: wtf?!
fromHex :: Fail.MonadFail m => ByteString -> m ByteString
fromHex hex = do
  let (str, rest) = Base16.decode $ bsToLower hex
  unless (ByteString.null rest) $
    fail $ "Can't decode hex" ++ show rest
  return str
  where
  bsToLower = ByteString.map $ fromIntegral
                     . fromEnum
                     . toLower
                     . toEnum
                     . fromIntegral

skipTillParser :: Parser a -> Parser a
skipTillParser p = P.choice [
  p,
  P.anyChar >> skipTillParser p
  ]
