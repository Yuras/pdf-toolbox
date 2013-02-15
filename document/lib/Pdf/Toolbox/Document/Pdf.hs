{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Basic implementation of pdf monad

module Pdf.Toolbox.Document.Pdf
(
  Pdf,
  Pdf',
  runPdf,
  runPdfWithHandle,
  document,
  flushObjectCache,
  getRIS
)
where

import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.IO

import Pdf.Toolbox.Core

import Pdf.Toolbox.Document.Monad
import Pdf.Toolbox.Document.Internal.Types

data PdfState = PdfState {
  stRIS :: RIS,
  stFilters :: [StreamFilter],
  stLastXRef :: Maybe XRef,
  stObjectCache :: Map Ref (Object Int64)
  }

-- | Basic implementation of pdf monad
newtype Pdf' m a = Pdf' (StateT PdfState m a)
  deriving (Monad, Functor, MonadIO, MonadTrans)

-- | Convenient type alias
type Pdf m a = PdfE (Pdf' m) a

instance MonadIO m => MonadPdf (Pdf' m) where
  lookupObject ref = do
    cached <- getFromCache ref
    case cached of
      Just o -> return o
      Nothing -> do
        xref <- getLastXRef
        entry <- lookupEntryRec ref xref
        o <- readObjectForEntry entry
        addObjectToCache ref o
        return o
  streamContent = takeStreamContent

readObjectForEntry :: MonadIO m => XRefEntry -> Pdf m (Object Int64)
readObjectForEntry (XRefTableEntry entry)
  | teIsFree entry = return ONull
  | otherwise = do
    ris <- getRIS
    readObjectAtOffset ris (teOffset entry) (teGen entry)
readObjectForEntry (XRefStreamEntry entry) =
  case entry of
    StreamEntryFree _ _ -> return ONull
    StreamEntryUsed off gen -> do
      ris <- getRIS
      readObjectAtOffset ris off gen
    StreamEntryCompressed _ _ -> left $ UnexpectedError "readObjectForEntry: compressed objects are not supported yet"

lookupEntryRec :: MonadIO m => Ref -> XRef -> Pdf m XRefEntry
lookupEntryRec ref = annotateError ("Can't find xref entry for ref: " ++ show ref) . loop
  where
  loop xref = do
    res <- lookupXRefEntry ref xref
    case res of
      Just e -> return e
      Nothing -> do
        ris <- getRIS
        prev <- prevXRef ris xref
        case prev of
          Just xref' -> loop xref'
          Nothing -> left $ UnexpectedError "There are no more xrefs"

lookupXRefEntry :: MonadIO m => Ref -> XRef -> Pdf m (Maybe XRefEntry)
lookupXRefEntry ref (XRefTable off) = do
  ris <- getRIS
  seek ris off
  _ <- inputStream ris >>= isTable
  pos <- tell ris
  fmap XRefTableEntry `liftM` lookupTableEntry ris pos ref
lookupXRefEntry ref (XRefStream s) = do
  decoded <- streamContent s
  fmap XRefStreamEntry `liftM` lookupStreamEntry decoded ref

takeStreamContent :: MonadIO m => Stream Int64 -> Pdf m (Stream IS)
takeStreamContent s@(Stream dict _) = annotateError ("reading stream content: " ++ show s) $ do
  len <- do
    obj <- lookupDict "Length" dict
    case obj of
      ONumber _ -> fromObject obj >>= intValue
      ORef ref -> lookupObject ref >>= fromObject >>= intValue
      _ -> left $ UnexpectedError $ "Unexpected length object in stream: " ++ show obj
  ris <- getRIS
  filters <- lift $ Pdf' $ gets stFilters
  decodedStreamContent ris filters len s

getLastXRef :: MonadIO m => Pdf m XRef
getLastXRef = do
  cached <- lift $ Pdf' $ gets stLastXRef
  case cached of
    Just xref -> return xref
    Nothing -> do
      xref <- getRIS >>= lastXRef
      lift $ Pdf' $ modify $ \st -> st {stLastXRef = Just xref}
      return xref

{-    st <- lift $ Pdf' get
    case Map.lookup ref (stObjectCache st) of
      Just o -> return o
      Nothing -> do
        xref <- case stLastXRef st of
                  Just xr -> return xr
                  Nothing -> do
                    xr <- lastXRef (stRIS st)
                    lift $ Pdf' $ put st {stLastXRef = Just xr}
                    return xr
        o <- Core.lookupObject (stRIS st) (stFilters st) xref lookupM ref
        addObjectToCache ref o
        return o
-}
--  streamContent s = do
--    undefined
{-
    ris <- getRIS
    filters <- lift $ Pdf' $ gets stFilters
    Core.streamContent ris filters lookupM s
-}
--  getRIS = lift $ Pdf' $ gets stRIS

-- | Access to the underlying random access input stream.
-- Can be used when you need to switch from high level
-- to low level of details
getRIS :: Monad m => Pdf m RIS
getRIS = lift $ Pdf' $ gets stRIS

--lookupM :: MonadIO m => Ref -> Pdf m (Object ())
--lookupM r = mapObject (const ()) `liftM` lookupObject r

getFromCache :: Monad m => Ref -> Pdf m (Maybe (Object Int64))
getFromCache ref = do
  cache <- lift $ Pdf' $ gets stObjectCache
  return $ Map.lookup ref cache

addObjectToCache :: Monad m => Ref -> Object Int64 -> Pdf m ()
addObjectToCache ref o = lift $ Pdf' $ modify $ \st ->
  st {stObjectCache = Map.insert ref o $ stObjectCache st}

-- | Remove all objects from cache
flushObjectCache :: Monad m => Pdf m ()
flushObjectCache = lift $ Pdf' $ modify $ \st -> st {stObjectCache = Map.empty}

-- | Execute PDF action with 'RIS'
runPdf :: MonadIO m => RIS -> [StreamFilter] -> Pdf m a -> m (Either PdfError a)
runPdf ris filters action = runPdf' ris filters $ runEitherT action

-- | Execute PDF action with 'Handle'
runPdfWithHandle :: MonadIO m => Handle -> [StreamFilter] -> Pdf m a -> m (Either PdfError a)
runPdfWithHandle handle filters action = do
  ris <- liftIO $ fromHandle handle
  runPdf ris filters action

runPdf' :: MonadIO m => RIS -> [StreamFilter] -> Pdf' m a -> m a
runPdf' ris filters (Pdf' action) = evalStateT action $ PdfState {
  stRIS = ris,
  stFilters = filters,
  stLastXRef = Nothing,
  stObjectCache = Map.empty
  }

-- | Get PDF document
document :: MonadIO m => Pdf m Document
document = do
  ris <- getRIS
  xref <- lastXRef ris
  tr <- trailer ris xref
  return $ Document tr
