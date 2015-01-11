{-# LANGUAGE OverloadedStrings #-}

-- | Document info dictionary

module Pdf.Toolbox.Document.Info
(
  infoTitle
)
where

import Data.ByteString (ByteString)

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Util

import Pdf.Toolbox.Document.Pdf
import Pdf.Toolbox.Document.Internal.Types

-- | Document title
infoTitle :: Info -> IO (Maybe ByteString)
infoTitle (Info pdf _ dict) =
  case lookupDict "Title" dict of
    Nothing -> return Nothing
    Just o -> do
      o' <- deref pdf o
      sure $ fmap Just (stringValue o') `notice` "Title should be a string"
