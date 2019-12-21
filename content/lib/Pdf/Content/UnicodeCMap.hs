{-# LANGUAGE OverloadedStrings #-}

-- | Unicode CMap defines mapping from glyphs to text

module Pdf.Content.UnicodeCMap
(
  UnicodeCMap(..),
  parseUnicodeCMap,
  unicodeCMapNextGlyph,
  unicodeCMapDecodeGlyph
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

-- | Take the next glyph code from string, also returns the rest of the string
unicodeCMapNextGlyph :: UnicodeCMap -> ByteString -> Maybe (Int, ByteString)
unicodeCMapNextGlyph cmap = go 1
  where
  go 5 _ = Nothing
  go n str =
    let glyph = ByteString.take n str in
    if ByteString.length glyph /= n
      then Nothing
      else if any (inRange glyph) (unicodeCMapCodeRanges cmap)
             then Just (toCode glyph, ByteString.drop n str)
             else go (n + 1) str
  inRange glyph (start, end) = glyph >= start && glyph <= end

toCode :: ByteString -> Int
toCode bs = fst $ ByteString.foldr (\b (sm, i) ->
                    (sm + fromIntegral b * i, i * 255)) (0, 1) bs

-- | Convert glyph to text
--
-- Note: one glyph can represent more then one char, e.g. for ligatures
unicodeCMapDecodeGlyph :: UnicodeCMap -> Int -> Maybe Text
unicodeCMapDecodeGlyph cmap glyph =
  case Map.lookup glyph (unicodeCMapChars cmap) of
    Just txt -> Just txt
    Nothing ->
      case filter inRange (unicodeCMapRanges cmap) of
        [(start, _, char)] -> Just (Text.singleton $ toEnum
                                    $ (fromEnum char) + (glyph - start))
        _ -> Nothing
  where
  inRange (start, end, _) = glyph >= start && glyph <= end

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
