{-# LANGUAGE OverloadedStrings #-}

-- | PDF document

module Pdf.Document.Document
(
  Document,
  documentCatalog,
  documentInfo,
  documentEncryption
)
where

import Pdf.Core.Object
import Pdf.Core.Object.Util
import Pdf.Core.Exception
import Pdf.Core.Util

import Pdf.Document.Pdf
import Pdf.Document.Internal.Types

import qualified Data.HashMap.Strict as HashMap
import Control.Exception hiding (throw)

dict :: Document -> Dict
dict (Document _ d) = d

pdf :: Document -> Pdf
pdf (Document p _) = p

-- | Get the document catalog
documentCatalog :: Document -> IO Catalog
documentCatalog doc = do
  ref <- sure $ (HashMap.lookup "Root" (dict doc) >>= refValue)
    `notice` "trailer: Root should be an indirect reference"
  obj <- lookupObject (pdf doc) ref
  d <- sure $ dictValue obj `notice` "catalog should be a dictionary"
  return (Catalog (pdf doc) ref d)

-- | Infornation dictionary for the document
documentInfo :: Document -> IO (Maybe Info)
documentInfo doc = do
  case HashMap.lookup "Info" (dict doc) of
    Nothing -> return Nothing
    Just (Ref ref) -> do
      obj <- lookupObject (pdf doc) ref
      d <- sure $ dictValue obj `notice` "info should be a dictionary"
      return (Just (Info (pdf doc) ref d))
    _ -> throwIO $ Corrupted "document Info should be an indirect reference" []

-- | Document encryption dictionary
documentEncryption :: Document -> IO (Maybe Dict)
documentEncryption doc = do
  case HashMap.lookup "Encrypt" (dict doc) of
    Nothing -> return Nothing
    Just o -> do
      o' <- deref (pdf doc) o
      case o' of
        Dict d -> return (Just d)
        Null -> return Nothing
        _ -> throwIO (Corrupted "document Encrypt should be a dictionary" [])
