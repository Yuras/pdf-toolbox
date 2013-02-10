{-# LANGUAGE OverloadedStrings #-}

-- | Flate decode filter

module Pdf.Toolbox.Stream.Filter.FlateDecode
(
  flateDecode
)
where

import Data.Word
import qualified Data.ByteString as BS
import Control.Error
import qualified System.IO.Streams as Streams

import Pdf.Toolbox.IO.RIS
import Pdf.Toolbox.Object.Types
import Pdf.Toolbox.Object.Util
import Pdf.Toolbox.Stream.Filter.Type

-- | Vary basic implementation. Only PNG-UP prediction is implemented
flateDecode :: StreamFilter
flateDecode = StreamFilter {
  filterName = "FlateDecode",
  filterDecode = decode
  }

decode :: Maybe Dict -> IS -> IO IS
decode Nothing is = Streams.decompress is
decode (Just dict) is = do
  predictor <- runEitherT $ lookupDict "Predictor" dict
  case predictor of
    Left _ -> Streams.decompress is
    Right p -> do
      p' <- runEitherT $ fromObject p >>= intValue
      case p' of
        Left e -> fail $ "Malformed predictor: " ++ show e
        Right val -> Streams.decompress is >>= unpredict dict val

unpredict :: Dict -> Int -> IS -> IO IS
unpredict _ 1 is = return is
unpredict dict 12 is = do
  c <- runEitherT $ lookupDict "Columns" dict >>= fromObject >>= intValue
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
