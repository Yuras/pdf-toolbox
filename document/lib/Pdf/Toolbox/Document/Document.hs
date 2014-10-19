{-# LANGUAGE OverloadedStrings #-}

-- | PDF document

module Pdf.Toolbox.Document.Document
(
  Document,
  catalog,
  info,
  encryption
)
where

import Control.Exception

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Util

import Pdf.Toolbox.Document.Pdf
import Pdf.Toolbox.Document.Internal.Types

dict :: Document -> Dict
dict (Document _ d) = d

pdf :: Document -> Pdf
pdf (Document p _) = p

-- | Get the document catalog
catalog :: Document -> IO Catalog
catalog doc = do
  ref <- sure $ (lookupDict "Root" (dict doc) >>= refValue)
    `notice` "trailer: Root should be an indirect reference"
  obj <- lookupObject (pdf doc) ref
  d <- sure $ dictValue obj `notice` "catalog should be a dictionary"
  return (Catalog (pdf doc) ref d)

-- | Infornation dictionary for the document
info :: Document -> IO (Maybe Info)
info doc = do
  case lookupDict "Info" (dict doc) of
    Nothing -> return Nothing
    Just (ORef ref) -> do
      obj <- lookupObject (pdf doc) ref
      d <- sure $ dictValue obj `notice` "info should be a dictionary"
      return (Just (Info (pdf doc) ref d))
    _ -> throw $ Corrupted "document Info should be an indirect reference" []

-- | Document encryption dictionary
encryption :: Document -> IO (Maybe Dict)
encryption doc = do
  case lookupDict "Encrypt" (dict doc) of
    Nothing -> return Nothing
    Just o -> do
      o' <- deref (pdf doc) o
      case o' of
        ODict d -> return (Just d)
        ONull -> return Nothing
        _ -> throw (Corrupted "document Encrypt should be a dictionary" [])
