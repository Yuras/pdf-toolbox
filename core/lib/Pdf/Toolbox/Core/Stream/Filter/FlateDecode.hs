{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Flate decode filter

module Pdf.Toolbox.Core.Stream.Filter.FlateDecode
(
  flateDecode
)
where

import Data.Word
import qualified Data.ByteString as BS
import Codec.Zlib
import Control.Error
import Control.Exception
import qualified System.IO.Streams as Streams

import Pdf.Toolbox.Core.IO
import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Object.Util
import Pdf.Toolbox.Core.Stream.Filter.Type

-- | Vary basic implementation. Only PNG-UP prediction is implemented
flateDecode :: StreamFilter
flateDecode = StreamFilter {
  filterName = "FlateDecode",
  filterDecode = \params is -> decode params is >>= catchZlibExceptions
  }

catchZlibExceptions :: IS -> IO IS
catchZlibExceptions is =
  Streams.makeInputStream $
    Streams.read is
    `catch` (\(e :: ZlibException) -> throwIO $ DecodeException $ toException e)

decode :: Maybe Dict -> IS -> IO IS
decode Nothing is = Streams.decompress is
decode (Just dict) is = do
  predictor <- runExceptT $ lookupDict "Predictor" dict
  case predictor of
    Left _ -> Streams.decompress is
    Right p -> do
      p' <- runExceptT $ fromObject p >>= intValue
      case p' of
        Left e -> fail $ "Malformed predictor: " ++ show e
        Right val -> Streams.decompress is >>= unpredict dict val

unpredict :: Dict -> Int -> IS -> IO IS
unpredict _ 1 is = return is
unpredict dict 12 is = do
  c <- runExceptT $ lookupDict "Columns" dict >>= fromObject >>= intValue
  case c of
    Left e -> fail $ "flateDecode: malformed Columns value: " ++ show e
    Right cols -> unpredict12 (cols + 1) is
unpredict _ p _ = fail $ "Unsupported predictor: " ++ show p

-- | PGN-UP prediction
--
-- TODO: Hacky solution, rewrite it
unpredict12 :: Int -> IS -> IO IS
unpredict12 cols is = Streams.toList is >>= Streams.fromList . return . BS.pack . step (replicate cols 0) [] . concatMap BS.unpack
  where
  step :: [Word8] -> [Word8] -> [Word8] -> [Word8]
  step _ _ [] = []
  step (c:cs) [] (_:xs) = step cs [c] xs
  step (c:cs) (p:ps) (x:xs) = (x + p) : step cs (c:(x + p):ps) xs
  step [] ps xs = step (reverse ps) [] xs
