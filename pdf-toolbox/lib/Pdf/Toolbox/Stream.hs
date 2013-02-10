{-# LANGUAGE  OverloadedStrings #-}

-- | Stream related tools

module Pdf.Toolbox.Stream
(
  rawStreamContent,
  streamContent,
  readStream,
  decodeStream
)
where

import Data.Int
import Data.Functor
import Control.Monad
import Control.Error

import Pdf.Toolbox.Object.Types
import Pdf.Toolbox.Object.Util
import Pdf.Toolbox.IO.RIS (RIS, IS)
import Pdf.Toolbox.IO.Monad
import Pdf.Toolbox.Parsers.Object
import Pdf.Toolbox.Stream.Filter.Type
import Pdf.Toolbox.Error

-- | Raw content of stream.
-- Filters are not applyed
--
-- The 'IS' is valid only until the next 'MonadPdfIO' operation
rawStreamContent :: MonadPdfIO m => RIS -> Stream Int64 -> PdfE m (Stream IS)
rawStreamContent ris (Stream dict off) = do
  seek ris off
  sz <- lookupDict "Length" dict >>= fromObject >>= intValue
  is <- inputStream ris >>= takeBytes (fromIntegral sz)
  return $ Stream dict is

-- | Decoded stream content
streamContent :: MonadPdfIO m => RIS -> [StreamFilter] -> Stream Int64 -> PdfE m (Stream IS)
streamContent ris filters s = rawStreamContent ris s >>= decodeStream filters

-- | Read 'Stream' at the current position in the 'RIS'
readStream :: MonadPdfIO m => RIS -> PdfE m (Stream Int64)
readStream ris = do
  Stream dict _ <- parseRIS ris parseIndirectObject >>= toStream . snd
  Stream dict <$> tell ris

-- | Decode stream content
decodeStream :: MonadPdfIO m => [StreamFilter] -> Stream IS -> PdfE m (Stream IS)
decodeStream filters (Stream dict istream) = annotateError "Can't decode stream" $ do
  list <- buildFilterList dict
  Stream dict <$> foldM decode istream list
  where
  decode is (name, params) = do
    f <- findFilter name
    applyFilter f params is
  findFilter name = tryHead (UnexpectedError $ "Filter not found: " ++ show name) $
    filter ((== name) . filterName) filters

buildFilterList :: Monad m => Dict -> PdfE m [(Name, Maybe Dict)]
buildFilterList dict = do
  f <- lookupDict "Filter" dict `catchT` (const $ right ONull)
  p <- lookupDict "DecodeParms" dict `catchT` (const $ right ONull)
  case (f, p) of
    (ONull, _) -> right []
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
    _ -> left $ UnexpectedError $ "Can't handle Filter and DecodeParams: (" ++ show f ++ ", " ++ show p ++ ")"
