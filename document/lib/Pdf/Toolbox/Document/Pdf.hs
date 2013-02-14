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
  flushCache
)
where

import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.IO

import Pdf.Toolbox.Core hiding (lookupObject)
import qualified Pdf.Toolbox.Core as Core

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
    st <- lift $ Pdf' get
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
  streamContent s = do
    ris <- getRIS
    filters <- lift $ Pdf' $ gets stFilters
    Core.streamContent ris filters lookupM s
  getRIS = lift $ Pdf' $ gets stRIS

lookupM :: MonadIO m => Ref -> Pdf m (Object ())
lookupM r = mapObject (const ()) `liftM` lookupObject r

addObjectToCache :: Monad m => Ref -> Object Int64 -> Pdf m ()
addObjectToCache ref o = lift $ Pdf' $ modify $ \st ->
  st {stObjectCache = Map.insert ref o $ stObjectCache st}

-- | Remove all objects from cache
flushCache :: Monad m => Pdf m ()
flushCache = lift $ Pdf' $ modify $ \st -> st {stObjectCache = Map.empty}

-- | Execute PDF action with 'RIS'
runPdf :: MonadIO m => RIS -> [StreamFilter] -> Pdf m a -> m (Either PdfError a)
runPdf ris filters action = runPdf' ris filters $ runEitherT action

-- | Execute PDF action with 'Handle'
runPdfWithHandle :: MonadIO m => Handle -> [StreamFilter] -> Pdf m a -> m (Either PdfError a)
runPdfWithHandle handle filters action = do
  ris <- liftIO $ fromHandle handle
  runPdf ris filters action

runPdf' :: MonadIO m => RIS -> [StreamFilter] -> Pdf' m a -> m a
runPdf' ris filters (Pdf' action) = evalStateT action $ PdfState ris filters Nothing Map.empty

-- | Get PDF document
document :: MonadIO m => Pdf m Document
document = do
  ris <- getRIS
  xref <- lastXRef ris
  tr <- trailer ris xref
  return $ Document tr
