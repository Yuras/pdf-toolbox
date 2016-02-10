{-# LANGUAGE OverloadedStrings #-}

-- | Document info dictionary

module Pdf.Document.Info
(
  infoTitle
)
where

import Pdf.Core
import Pdf.Core.Util

import Pdf.Document.Pdf
import Pdf.Document.Internal.Types

import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HashMap

-- | Document title
infoTitle :: Info -> IO (Maybe ByteString)
infoTitle (Info pdf _ dict) =
  case HashMap.lookup "Title" dict of
    Nothing -> return Nothing
    Just o -> do
      o' <- deref pdf o
      sure $ fmap Just (stringValue o') `notice` "Title should be a string"
