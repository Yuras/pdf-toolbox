{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for internal use

module Pdf.Toolbox.Document.Internal.Util
(
  ensureType,
  dictionaryType
)
where

import Control.Monad

import Pdf.Toolbox.Core

-- | Check that the dictionary has the specified \"Type\" filed
ensureType :: Monad m => Name -> Dict -> PdfE m ()
ensureType name dict = do
  n <- dictionaryType dict
  unless (n == name) $ left $ UnexpectedError $ "Expected type: " ++ show name ++ ", but found: " ++ show n

-- | Get dictionary type, name at key \"Type\"
dictionaryType :: Monad m => Dict -> PdfE m Name
dictionaryType dict = lookupDict "Type" dict >>= fromObject
