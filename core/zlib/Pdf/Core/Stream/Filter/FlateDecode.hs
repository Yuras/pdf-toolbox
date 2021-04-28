{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

-- | Flate decode filter

module Pdf.Core.Stream.Filter.FlateDecode
(
  flateDecode
)
where

import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap
import Control.Exception hiding (throw)
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams

import Pdf.Core.Exception
import Pdf.Core.Object
import Pdf.Core.Object.Util
import Pdf.Core.Stream.Filter.Type

-- | Vary basic implementation. Only PNG-UP prediction is implemented
--
-- Nothing when zlib is disabled via cabal flag
flateDecode :: Maybe StreamFilter
flateDecode = Just StreamFilter
  { filterName = "FlateDecode"
  , filterDecode = decode
  }

decode :: Maybe Dict -> InputStream ByteString -> IO (InputStream ByteString)
decode Nothing is = Streams.decompress is
decode (Just dict) is =
  case HashMap.lookup "Predictor" dict of
    Nothing -> Streams.decompress is
    Just o | Just val <- intValue o ->
      Streams.decompress is >>= unpredict dict val
    _ -> throwIO $ Corrupted "Predictor should be an integer" []

unpredict :: Dict
          -> Int
          -> InputStream ByteString
          -> IO (InputStream ByteString)
unpredict _ 1 is = return is
unpredict dict 12 is = message "unpredict" $
  case HashMap.lookup "Columns" dict of
    Nothing -> throwIO $ Corrupted "Column is missing" []
    Just o
      | Just cols <- intValue o
      -> unpredict12 (cols + 1) is
    _ -> throwIO $ Corrupted "Column should be an integer" []
unpredict _ p _ = throwIO $ Unexpected ("Unsupported predictor: " ++ show p) []

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
