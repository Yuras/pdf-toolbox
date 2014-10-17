{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Flate decode filter

module Pdf.Toolbox.Core.Stream.Filter.FlateDecode
(
  flateDecode
)
where

import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Control.Exception
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams

import Pdf.Toolbox.Core.Exception
import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Object.Util
import Pdf.Toolbox.Core.Stream.Filter.Type

-- | Vary basic implementation. Only PNG-UP prediction is implemented
flateDecode :: StreamFilter
flateDecode = StreamFilter
  { filterName = "FlateDecode"
  , filterDecode = decode
  }

decode :: Maybe Dict -> InputStream ByteString -> IO (InputStream ByteString)
decode Nothing is = Streams.decompress is
decode (Just dict) is =
  case lookupDict "Predictor" dict of
    Nothing -> Streams.decompress is
    Just (ONumber (NumInt val)) ->
      Streams.decompress is >>= unpredict dict val
    _ -> throw $ Corrupted "Predictor should be an integer" []

unpredict :: Dict
          -> Int
          -> InputStream ByteString
          -> IO (InputStream ByteString)
unpredict _ 1 is = return is
unpredict dict 12 is = message "unpredict" $
  case lookupDict "Columns" dict of
    Nothing -> throw $ Corrupted "Column is missing" []
    Just (ONumber (NumInt cols)) -> unpredict12 (cols + 1) is
    _ -> throw $ Corrupted "Column should be an integer" []
unpredict _ p _ = throw $ Unexpected ("Unsupported predictor: " ++ show p) []

-- | PGN-UP prediction
--
-- TODO: Hacky solution, rewrite it
unpredict12 :: Int -> InputStream ByteString -> IO (InputStream ByteString)
unpredict12 cols is
  = Streams.toList is
  >>= Streams.fromList . return
                       . ByteString.pack
                       . step (replicate cols 0) []
                       . concatMap ByteString.unpack
  where
  step :: [Word8] -> [Word8] -> [Word8] -> [Word8]
  step _ _ [] = []
  step (c:cs) [] (_:xs) = step cs [c] xs
  step (c:cs) (p:ps) (x:xs) = (x + p) : step cs (c:(x + p):ps) xs
  step [] ps xs = step (reverse ps) [] xs
