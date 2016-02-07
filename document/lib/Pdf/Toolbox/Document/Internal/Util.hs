{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for internal use

module Pdf.Toolbox.Document.Internal.Util
(
  ensureType,
  dictionaryType
)
where

import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Exception hiding (throw)

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Name (Name)

-- | Check that the dictionary has the specified \"Type\" filed
ensureType :: Name -> Dict -> IO ()
ensureType name dict = do
  n <- sure $ dictionaryType dict
  unless (n == name) $
    throwIO $ Corrupted ("Expected type: " ++ show name ++
                       ", but found: " ++ show n) []

-- | Get dictionary type, name at key \"Type\"
dictionaryType :: Dict -> Either String Name
dictionaryType dict =
  case HashMap.lookup "Type" dict of
    Just (Name n) -> Right n
    Just _ -> Left "Type should be a name"
    _ -> Left "Type is missing"
