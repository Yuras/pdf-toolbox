{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for internal use

module Pdf.Toolbox.Document.Internal.Util
(
  ensureType,
  dictionaryType
)
where

import Control.Monad
import Control.Exception

import Pdf.Toolbox.Core

-- | Check that the dictionary has the specified \"Type\" filed
ensureType :: Name -> Dict -> IO ()
ensureType name dict = do
  n <- sure $ dictionaryType dict
  unless (n == name) $
    throw $ Corrupted ("Expected type: " ++ show name ++
                       ", but found: " ++ show n) []

-- | Get dictionary type, name at key \"Type\"
dictionaryType :: Dict -> Either String Name
dictionaryType dict =
  case lookupDict "Type" dict of
    Just (OName n) -> Right n
    Just _ -> Left "Type should be a name"
    _ -> Left "Type is missing"
