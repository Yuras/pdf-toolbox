{-# LANGUAGE  OverloadedStrings #-}

-- | Stream related tools

module Pdf.Toolbox.Core.Stream
(
  StreamFilter,
  knownFilters,
  readStream,
  rawStreamContent,
  decodedStreamContent,
  decodeStream
)
where

import Data.Int
import Data.Maybe
import Data.ByteString (ByteString)
import Control.Applicative
import Control.Monad
import Control.Exception
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import Pdf.Toolbox.Core.IO.Buffer
import Pdf.Toolbox.Core.Exception
import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Object.Util
import Pdf.Toolbox.Core.Parsers.Object
import Pdf.Toolbox.Core.Stream.Filter.Type
import Pdf.Toolbox.Core.Stream.Filter.FlateDecode

-- | Read 'Stream' from stream
--
-- We need to pass current position here to calculate stream data offset
readStream :: InputStream ByteString -> Int64 -> IO (Stream Int64)
readStream is off = do
  (is', counter) <- Streams.countInput is
  (_, obj) <- Streams.parseFromStream parseIndirectObject is'
  case obj of
    OStream (Stream dict _) -> Stream dict . (+off) . fromIntegral <$> counter
    _ -> throw $ Streams.ParseException ("stream expected, but got: "
                                          ++ show obj)

-- | All stream filters implemented by the toolbox
--
-- Right now it contains only FlateDecode filter
knownFilters :: [StreamFilter]
knownFilters = [flateDecode]

-- | Raw stream content.
-- Filters are not applyed
--
-- The 'InputStream' returned is valid only until the next 'bufferSeek'
--
-- Note: \"Length\" could be an indirect object, but we don't want
-- to read indirect objects here. So we require length to be provided
rawStreamContent :: Buffer
                 -> Int           -- ^ stream length
                 -> Stream Int64  -- ^ stream object to read content for.
                                  -- The payload is offset of stream data
                 -> IO (InputStream ByteString)
rawStreamContent buf len (Stream _ off) = do
  bufferSeek buf off
  Streams.takeBytes (fromIntegral len) (bufferToInputStream buf)

-- | Decode stream content
--
-- It should be already decrypted
--
-- The 'InputStream' is valid only until the next 'bufferSeek'
decodeStream :: [StreamFilter]
             -> Stream (InputStream ByteString)
             -> IO (InputStream ByteString)
decodeStream filters (Stream dict istream) =
  buildFilterList dict >>= foldM decode istream
  where
  decode is (name, params) = do
    f <- findFilter name
    filterDecode f params is
  findFilter name =
    case filter ((== name) . filterName) filters of
      [] -> throw $ Corrupted "Filter not found" []
      (f : _) -> return f

buildFilterList :: Dict -> IO [(Name, Maybe Dict)]
buildFilterList dict = do
  let f = fromMaybe ONull $ lookupDict "Filter" dict
      p = fromMaybe ONull $ lookupDict "DecodeParams" dict
  case (f, p) of
    (ONull, _) -> return []
    (OName fd, ONull) -> return [(fd, Nothing)]
    (OName fd, ODict pd) -> return [(fd, Just pd)]
    (OName fd, OArray (Array [ODict pd])) -> return [(fd, Just pd)]
    (OArray (Array fa), ONull) -> do
      fa' <- forM fa $ \o ->
        case o of
          OName n -> return n
          _ -> throw $ Corrupted ("Filter should be a Name") []
      return $ zip fa' (repeat Nothing)
    (OArray (Array fa), OArray (Array pa)) | length fa == length pa -> do
      fa' <- forM fa $ \o ->
        case o of
          OName n -> return n
          _ -> throw $ Corrupted ("Filter should be a Name") []
      pa' <- forM pa $ \o ->
        case o of
          ODict d -> return d
          _ -> throw $ Corrupted ("DecodeParams should be a dictionary") []
      return $ zip fa' (map Just pa')
    _ -> throw $ Corrupted ("Can't handle Filter and DecodeParams: ("
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
                     -> Stream Int64
                     -- ^ stream with offset
                     -> IO (InputStream ByteString)
decodedStreamContent buf filters decryptor len s@(Stream dict _) =
  rawStreamContent buf len s >>=
  decryptor >>=
  decodeStream filters . Stream dict
