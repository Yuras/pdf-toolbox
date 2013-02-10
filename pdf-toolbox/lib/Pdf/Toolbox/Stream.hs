{-# LANGUAGE  OverloadedStrings #-}

-- | Stream related tools

module Pdf.Toolbox.Stream
(
  rawStreamContent,
  readStream
)
where

import Data.Int
import Data.Functor

import Pdf.Toolbox.Object.Types
import Pdf.Toolbox.Object.Util
import Pdf.Toolbox.IO.RIS (RIS, IS)
import Pdf.Toolbox.IO.Monad
import Pdf.Toolbox.Parsers.Object
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

-- | Read 'Stream' at the current position in the 'RIS'
readStream :: MonadPdfIO m => RIS -> PdfE m (Stream Int64)
readStream ris = do
  Stream dict _ <- parseRIS ris parseIndirectObject >>= toStream . snd
  Stream dict <$> tell ris
