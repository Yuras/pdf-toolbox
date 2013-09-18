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
  withoutObjectCache,
  getRIS,
  knownFilters,
  isEncrypted,
  setUserPassword,
  defaultUserPassord,
  decrypt,
  getDecryptor,
  MonadIO(..)
)
where

import Data.Monoid
import Data.Int
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.IO
import qualified System.IO.Streams as Streams

import Pdf.Toolbox.Core

import Pdf.Toolbox.Document.Monad
import Pdf.Toolbox.Document.Encryption
import Pdf.Toolbox.Document.Internal.Types

data PdfState = PdfState {
  stRIS :: RIS,
  stFilters :: [StreamFilter],
  stLastXRef :: Maybe XRef,
  stAddToObjectCache :: Bool,
  stObjectCache :: Map Ref (Object Int64),
  stXRefStreamCache :: Map Int64 [ByteString],
  stDecryptor :: Maybe Decryptor
  }

-- | Basic implementation of pdf monad
newtype Pdf' m a = Pdf' (StateT PdfState m a)
  deriving (Monad, Functor, MonadIO, MonadTrans)

-- | Convenient type alias
type Pdf m = PdfE (Pdf' m)

instance MonadIO m => MonadPdf (Pdf' m) where
  lookupObject ref = annotateError ("lookupObject: " ++ show ref) $ do
    cached <- getFromCache ref
    case cached of
      Just o -> return o
      Nothing -> do
        xref <- getLastXRef
        entry <- lookupEntryRec ref xref
        o <- readObjectForEntry ref entry
        addObjectToCache ref o
        return o
  streamContent ref s = do
    decryptor <- do
      dec <- getDecryptor
      case dec of
        Nothing -> return return
        Just d -> return $ d ref
    takeStreamContent decryptor s

readObjectForEntry :: MonadIO m => Ref -> XRefEntry -> Pdf m (Object Int64)
readObjectForEntry ref (XRefTableEntry entry)
  | teIsFree entry = return ONull
  | otherwise = do
    ris <- getRIS
    readObjectAtOffset ris (teOffset entry) (teGen entry) >>= decrypt ref
readObjectForEntry ref (XRefStreamEntry entry) =
  case entry of
    StreamEntryFree _ _ -> return ONull
    StreamEntryUsed off gen -> do
      ris <- getRIS
      readObjectAtOffset ris off gen >>= decrypt ref
    StreamEntryCompressed index num -> do
      objStream <- lookupObject (Ref index 0) >>= toStream
      Stream dict is <- streamContent (Ref index 0) objStream
      first <- lookupDict "First" dict >>= fromObject >>= intValue
      mapObject (error "readObjectForEntry: impossible") `liftM`
        readCompressedObject is (fromIntegral first) num

getXRefStream :: MonadIO m => Stream Int64 -> Pdf m (Stream IS)
getXRefStream s@(Stream dict off) = do
  cache <- lift $ Pdf' $ gets stXRefStreamCache
  content <-
    case Map.lookup off cache of
      Just content -> return content
      Nothing -> do
        Stream _ is <- takeStreamContent return s
        content <- liftIO $ Streams.toList is
        lift $ Pdf' $ modify $ \st -> st {stXRefStreamCache = Map.insert off content $ stXRefStreamCache st}
        return content
  is <- liftIO $ Streams.fromList content
  return $ Stream dict is

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
  fmap XRefTableEntry `liftM` lookupTableEntry ris ref
lookupXRefEntry ref (XRefStream _ s) = do
  decoded <- getXRefStream s
  fmap XRefStreamEntry `liftM` lookupStreamEntry decoded ref

takeStreamContent :: MonadIO m => (IS -> IO IS) -> Stream Int64 -> Pdf m (Stream IS)
takeStreamContent decryptor s@(Stream dict _) = annotateError ("reading stream content: " ++ show s) $ do
  len <- do
    obj <- lookupDict "Length" dict
    case obj of
      ONumber _ -> fromObject obj >>= intValue
      ORef ref -> lookupObject ref >>= fromObject >>= intValue
      _ -> left $ UnexpectedError $ "Unexpected length object in stream: " ++ show obj
  ris <- getRIS
  filters <- lift $ Pdf' $ gets stFilters
  decodedStreamContent ris filters decryptor len s

getLastXRef :: MonadIO m => Pdf m XRef
getLastXRef = do
  cached <- lift $ Pdf' $ gets stLastXRef
  case cached of
    Just xref -> return xref
    Nothing -> do
      xref <- getRIS >>= lastXRef
      lift $ Pdf' $ modify $ \st -> st {stLastXRef = Just xref}
      return xref

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
addObjectToCache ref o = do
  add <- lift $ Pdf' $ gets stAddToObjectCache
  when add $
    lift $ Pdf' $ modify $ \st ->
      st {stObjectCache = Map.insert ref o $ stObjectCache st}

-- | Perform action without adding objects to cache.
-- Note: the existent cache is not flushed, and is used
-- within the action
withoutObjectCache :: Monad m => Pdf m () -> Pdf m ()
withoutObjectCache action = do
  old <- lift $ Pdf' $ do
    val <- gets stAddToObjectCache
    modify $ \st -> st {stAddToObjectCache = False}
    return val
  action
  lift $ Pdf' $ modify $ \st -> st {stAddToObjectCache = old}

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
  stObjectCache = Map.empty,
  stXRefStreamCache = Map.empty,
  stDecryptor = Nothing,
  stAddToObjectCache = True
  }

-- | Get PDF document
document :: MonadIO m => Pdf m Document
document = do
  ris <- getRIS
  xref <- lastXRef ris
  tr <- trailer ris xref
  return $ Document xref tr

-- | Whether the PDF document it encrypted
isEncrypted :: MonadIO m => Pdf m Bool
isEncrypted = annotateError "isEncrypted" $ do
  ris <- getRIS
  tr <- lastXRef ris >>= trailer ris
  case lookupDict' "Encrypt" tr of
    Nothing -> return False
    Just _ -> return True

-- | Set the password to be user for decryption
--
-- Returns False when the password is wrong
setUserPassword :: MonadIO m => ByteString -> Pdf m Bool
setUserPassword pass = annotateError "setUserPassword" $ do
  ris <- getRIS
  tr <- lastXRef ris >>= trailer ris
  enc <- case lookupDict' "Encrypt" tr of
    Nothing -> left $ UnexpectedError "The document is not encrypted"
    Just enc -> deref enc >>= fromObject
  decryptor <- mkStandardDecryptor tr enc $ BS.take 32 $ pass `mappend` defaultUserPassord
  lift $ Pdf' $ modify $ \s -> s {stDecryptor = decryptor}
  case decryptor of
    Nothing -> return False
    _ -> return True

-- | Decryptor
getDecryptor :: Monad m => Pdf m (Maybe Decryptor)
getDecryptor = lift $ Pdf' $ gets stDecryptor

-- | Decrypt PDF object using user password is set
decrypt :: MonadIO m => Ref -> Object a -> Pdf m (Object a)
decrypt ref o = do
  decryptor <- getDecryptor
  case decryptor of
    Nothing -> return o
    Just decr -> liftIO $ decryptObject (decr ref) o
