{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

-- | Stream related tools

module Pdf.Core.Stream
(
  StreamFilter,
  knownFilters,
  readStream,
  rawStreamContent,
  decodedStreamContent,
  decodeStream
)
where

import Pdf.Core.Exception
import Pdf.Core.Object
import Pdf.Core.Parsers.Object
import Pdf.Core.Stream.Filter.Type
import Pdf.Core.Stream.Filter.FlateDecode
import Pdf.Core.IO.Buffer (Buffer)
import qualified Pdf.Core.IO.Buffer as Buffer

import Data.Int
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Exception hiding (throw)
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

-- | Read 'Stream' from stream
--
-- We need to pass current position here to calculate stream data offset
readStream :: InputStream ByteString -> Int64 -> IO Stream
readStream is off = do
  (is', counter) <- Streams.countInput is
  (_, obj) <- Streams.parseFromStream parseIndirectObject is'
    `catch` \(Streams.ParseException msg) -> throwIO (Corrupted msg [])
  case obj of
    Stream (S dict _) -> do
      off' <- counter
      return (S dict (off + off'))
    _ -> throwIO $ Streams.ParseException ("stream expected, but got: "
                                          ++ show obj)

-- | All stream filters implemented by the toolbox
--
-- Right now it contains only FlateDecode filter
knownFilters :: [StreamFilter]
knownFilters = catMaybes [flateDecode]

-- | Raw stream content.
-- Filters are not applyed
--
-- The 'InputStream' returned is valid only until the next 'bufferSeek'
--
-- Note: \"Length\" could be an indirect object, but we don't want
-- to read indirect objects here. So we require length to be provided
rawStreamContent :: Buffer
                 -> Int           -- ^ stream length
                 -> Int64         -- ^ stream offset
                                  -- The payload is offset of stream data
                 -> IO (InputStream ByteString)
rawStreamContent buf len off = do
  Buffer.seek buf off
  Streams.takeBytes (fromIntegral len) (Buffer.toInputStream buf)

-- | Decode stream content
--
-- It should be already decrypted
--
-- The 'InputStream' is valid only until the next 'bufferSeek'
decodeStream :: [StreamFilter]
             -> Stream -> InputStream ByteString
             -> IO (InputStream ByteString)
decodeStream filters (S dict _) istream =
  buildFilterList dict >>= foldM decode istream
  where
  decode is (name, params) = do
    f <- findFilter name
    filterDecode f params is
  findFilter name =
    case filter ((== name) . filterName) filters of
      [] -> throwIO $ Corrupted "Filter not found" []
      (f : _) -> return f

buildFilterList :: Dict -> IO [(Name, Maybe Dict)]
buildFilterList dict = do
  let f = fromMaybe Null $ HashMap.lookup "Filter" dict
      p = fromMaybe Null $ HashMap.lookup "DecodeParms" dict
  case (f, p) of
    (Null, _) -> return []
    (Name fd, Null) -> return [(fd, Nothing)]
    (Name fd, Dict pd) -> return [(fd, Just pd)]
    (Name fd, Array arr)
      | [Dict pd] <- Vector.toList arr
      -> return [(fd, Just pd)]
    (Array fa, Null) -> do
      fa' <- forM (Vector.toList fa) $ \o ->
        case o of
          Name n -> return n
          _ -> throwIO $ Corrupted ("Filter should be a Name") []
      return $ zip fa' (repeat Nothing)
    (Array fa, Array pa) | Vector.length fa == Vector.length pa -> do
      fa' <- forM (Vector.toList fa) $ \o ->
        case o of
          Name n -> return n
          _ -> throwIO $ Corrupted ("Filter should be a Name") []
      pa' <- forM (Vector.toList pa) $ \o ->
        case o of
          Dict d -> return d
          _ -> throwIO $ Corrupted ("DecodeParams should be a dictionary") []
      return $ zip fa' (map Just pa')
    _ -> throwIO $ Corrupted ("Can't handle Filter and DecodeParams: ("
                            ++ show f ++ ", " ++ show p ++ ")") []

-- | Decoded stream content
--
-- The 'InputStream' is valid only until the next 'bufferSeek'
--
-- Note: \"Length\" could be an indirect object, that is why
-- we cann't read it ourself
decodedStreamContent :: Buffer
                     -> [StreamFilter]
                     -> (InputStream ByteString -> IO (InputStream ByteString))
                     -- ^ decryptor
                     -> Int
                     -- ^ stream length
                     -> Stream
                     -- ^ stream with offset
                     -> IO (InputStream ByteString)
decodedStreamContent buf filters decryptor len s@(S _ off) =
  rawStreamContent buf len off >>=
  decryptor >>=
  decodeStream filters s
