{-# LANGUAGE OverloadedStrings #-}

-- | Unicode CMap defines mapping from glyphs to text

module Pdf.Toolbox.Content.UnicodeCMap
(
  UnicodeCMap(..),
  parseUnicodeCMap,
  unicodeCMapNextGlyph,
  unicodeCMapDecodeGlyph
)
where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Attoparsec.Char8 (Parser, parseOnly)
import qualified Data.Attoparsec.Char8 as P
import Control.Monad

-- | Unicode character map
--
-- Font dictionary can contain \"ToUnicode\" key -- reference
-- to a stream with unicode CMap
data UnicodeCMap = UnicodeCMap {
  unicodeCMapCodeRanges :: [(ByteString, ByteString)],
  unicodeCMapChars :: Map ByteString Text,
  unicodeCMapRanges :: [(ByteString, ByteString, Char)]
  }
  deriving (Show)

-- | Parse content of unicode CMap
parseUnicodeCMap :: ByteString -> Either String UnicodeCMap
parseUnicodeCMap cmap =
  case (codeRanges, chars, ranges) of
    (Right cr, Right cs, Right rs) -> Right $ UnicodeCMap {
      unicodeCMapCodeRanges = cr,
      unicodeCMapChars = cs,
      unicodeCMapRanges = rs
      }
    (Left err, _, _) -> Left $ "CMap code ranges: " ++ err
    (_, Left err, _) -> Left $ "CMap chars: " ++ err
    (_, _, Left err) -> Left $ "CMap ranges: " ++ err
  where
  codeRanges = parseOnly codeRangesParser cmap
  chars = parseOnly charsParser cmap
  ranges = parseOnly rangesParser cmap

-- | Take the next glyph from string, also returns the rest of the string
unicodeCMapNextGlyph :: UnicodeCMap -> ByteString -> Maybe (ByteString, ByteString)
unicodeCMapNextGlyph cmap = go 1
  where
  go 5 _ = Nothing
  go n str =
    let glyph = BS.take n str in
    if BS.length glyph /= n
      then Nothing
      else if any (inRange glyph) (unicodeCMapCodeRanges cmap)
             then Just (glyph, BS.drop n str)
             else go (n + 1) str
  inRange glyph (start, end) = glyph >= start && glyph <= end

-- | Convert glyph to text
--
-- Note: one glyph can represent more then one char, e.g. for ligatures
unicodeCMapDecodeGlyph :: UnicodeCMap -> ByteString -> Maybe Text
unicodeCMapDecodeGlyph cmap glyph =
  case Map.lookup glyph (unicodeCMapChars cmap) of
    Just txt -> Just txt
    Nothing ->
      case filter inRange (unicodeCMapRanges cmap) of
        [(start, _, char)] -> Just (Text.singleton $ toEnum $ (fromEnum char) + bsDiff glyph start)
        _ -> Nothing
  where
  inRange (start, end, _) = glyph >= start && glyph <= end
  bsDiff bs1 bs2 = snd $ foldr (\v (i, s) -> (i * 255, s + fromIntegral v * i)) (1 :: Int, 0) $ map (uncurry (-)) $ BS.zip bs1 bs2

charsParser :: Parser (Map ByteString Text)
charsParser = do
  n <- P.option 0 $ skipTillParser $ do
    n <- P.decimal
    P.skipSpace
    _ <- P.string "beginbfchar"
    return n

  chars <- replicateM n $ do
    P.skipSpace
    _ <- P.char '<'
    i <- P.takeTill (== '>') >>= fromHex
    _ <- P.char '>'
    P.skipSpace
    _ <- P.char '<'
    j <- P.takeTill (== '>') >>= fromHex
    _ <- P.char '>'
    return (i, Text.decodeUtf16BE j)

  return $ Map.fromList chars

rangesParser :: Parser [(ByteString, ByteString, Char)]
rangesParser = do
  n <- P.option 0 $ skipTillParser $ do
    n <- P.decimal
    P.skipSpace
    _ <- P.string "beginbfrange"
    return n

  replicateM n $ do
    P.skipSpace
    _ <- P.char '<'
    i <- P.takeTill (== '>') >>= fromHex
    _ <- P.char '>'
    P.skipSpace
    _ <- P.char '<'
    j <- P.takeTill (== '>') >>= fromHex
    _ <- P.char '>'
    P.skipSpace
    _ <- P.char '<'
    k <- P.takeTill (== '>') >>= fromHex
    _ <- P.char '>'
    return (i, j, Text.head $ Text.decodeUtf16BE k)

codeRangesParser :: Parser [(ByteString, ByteString)]
codeRangesParser = do
  n <- skipTillParser $ do
    n <- P.decimal
    P.skipSpace
    _ <- P.string "begincodespacerange"
    return n

  replicateM n $ do
    P.skipSpace
    _ <- P.char '<'
    i <- P.takeTill (== '>') >>= fromHex
    _ <- P.char '>'
    P.skipSpace
    _ <- P.char '<'
    j <- P.takeTill (== '>') >>= fromHex
    _ <- P.char '>'
    return (i, j)

fromHex :: Monad m => ByteString -> m ByteString
fromHex hex = do
  let (str, rest) = Base16.decode $ bsToLower hex
  unless (BS.null rest) $
    fail $ "Can't decode hex" ++ show rest
  return str
  where
  bsToLower = BS.map $ fromIntegral . fromEnum . toLower . toEnum . fromIntegral

skipTillParser :: Parser a -> Parser a
skipTillParser p = P.choice [
  p,
  P.anyChar >> skipTillParser p
  ]
