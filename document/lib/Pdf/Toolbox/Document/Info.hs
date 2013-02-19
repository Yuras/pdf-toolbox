{-# LANGUAGE OverloadedStrings #-}

-- | Document info dictionary

module Pdf.Toolbox.Document.Info
(
  infoTitle
)
where

import Control.Monad

import Pdf.Toolbox.Core

import Pdf.Toolbox.Document.Monad
import Pdf.Toolbox.Document.Internal.Types

-- | Document title
infoTitle :: MonadPdf m => Info -> PdfE m (Maybe Str)
infoTitle (Info _ dict) =
  case lookupDict' "Title" dict of
    Nothing -> return Nothing
    Just o -> Just `liftM` fromObject o
