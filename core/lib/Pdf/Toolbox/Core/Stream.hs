{-# LANGUAGE  OverloadedStrings #-}

-- | Stream related tools

module Pdf.Toolbox.Core.Stream
(
  StreamFilter,
  knownFilters,
  rawStreamContent,
  decodedStreamContent,
  readStream,
  decodeStream
)
where

import Data.Int
import Control.Monad

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Object.Util
import Pdf.Toolbox.Core.IO
import Pdf.Toolbox.Core.Parsers.Object
import Pdf.Toolbox.Core.Stream.Filter.Type
import Pdf.Toolbox.Core.Stream.Filter.FlateDecode
import Pdf.Toolbox.Core.Error

-- | All stream filters implemented by the toolbox
--
-- Right now it contains only FlateDecode filter
knownFilters :: [StreamFilter]
knownFilters = [flateDecode]

-- | Raw content of stream.
-- Filters are not applyed
--
-- The 'IS' is valid only until the next 'seek'
--
-- Note: \"Length\" could be an indirect object, but we don't want
-- to read indirect objects here. So we require length to be provided
rawStreamContent :: MonadIO m
                 => RIS                 -- ^ random access input stream to read from
                 -> Int                 -- ^ stream length
                 -> Stream Int64        -- ^ stream object to read content for.
                                        -- The payload is offset of stream data
                 -> PdfE m (Stream IS)  -- ^ resulting stream object
rawStreamContent ris len (Stream dict off) = annotateError ("reading raw stream content at offset: " ++ show off) $ do
  seek ris off
  is <- inputStream ris >>= takeBytes (fromIntegral len)
  return $ Stream dict is

-- | Decoded stream content
--
-- The 'IS' is valid only until the next 'seek'
--
-- Note: \"Length\" could be an indirect object, that is why
-- we cann't read it ourself
decodedStreamContent :: MonadIO m
                     => RIS                -- ^ random input stream to read from
                     -> [StreamFilter]     -- ^ stream filters
                     -> (IS -> IO IS)      -- ^ decryptor
                     -> Int                -- ^ stream length
                     -> Stream Int64       -- ^ stream with offset
                     -> PdfE m (Stream IS)
decodedStreamContent ris filters decryptor len s = rawStreamContent ris len s >>= decodeStream filters decryptor

-- | Read 'Stream' at the current position in the 'RIS'
readStream :: MonadIO m => RIS -> PdfE m (Stream Int64)
readStream ris = do
  Stream dict _ <- inputStream ris >>= parse parseIndirectObject >>= toStream . snd
  Stream dict `liftM` tell ris

-- | Decode stream content
--
-- The 'IS' is valid only until the next 'RIS' operation
decodeStream :: MonadIO m => [StreamFilter] -> (IS -> IO IS) -> Stream IS -> PdfE m (Stream IS)
decodeStream filters decryptor (Stream dict istream) = annotateError "Can't decode stream" $ do
  is <- liftIO $ decryptor istream
  list <- buildFilterList dict
  Stream dict `liftM` foldM decode is list
  where
  decode is (name, params) = do
    f <- findFilter name
    tryPdfIO $ filterDecode f params is
  findFilter name = tryHead (UnexpectedError $ "Filter not found: " ++ show name) $
    filter ((== name) . filterName) filters

buildFilterList :: Monad m => Dict -> PdfE m [(Name, Maybe Dict)]
buildFilterList dict = do
  f <- lookupDict "Filter" dict `catchE` (const $ return ONull)
  p <- lookupDict "DecodeParms" dict `catchE` (const $ return ONull)
  case (f, p) of
    (ONull, _) -> return []
    (OName fd, ONull) -> return [(fd, Nothing)]
    (OName fd, ODict pd) -> return [(fd, Just pd)]
    (OName fd, OArray (Array [ODict pd])) -> return [(fd, Just pd)]
    (OArray (Array fa), ONull) -> do
      fa' <- mapM fromObject fa
      return $ zip fa' (repeat Nothing)
    (OArray (Array fa), OArray (Array pa)) | length fa == length pa -> do
      fa' <- mapM fromObject fa
      pa' <- mapM fromObject pa
      return $ zip fa' (map Just pa')
    _ -> throwE $ UnexpectedError $ "Can't handle Filter and DecodeParams: (" ++ show f ++ ", " ++ show p ++ ")"
