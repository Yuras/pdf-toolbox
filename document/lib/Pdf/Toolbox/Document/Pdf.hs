{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Basic implementation of pdf monad

module Pdf.Toolbox.Document.Pdf
(
  Pdf,
  Pdf',
  runPdf,
  runPdfWithHandle,
  document
)
where

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
  stFilters :: [StreamFilter]
  }

-- | Basic implementation of pdf monad
newtype Pdf' m a = Pdf' (StateT PdfState m a)
  deriving (Monad, Functor, MonadIO, MonadTrans)

-- | Convenient type alias
type Pdf m a = PdfE (Pdf' m) a

instance MonadIO m => MonadPdf (Pdf' m) where
  lookupObject ref = do
    st <- lift $ Pdf' get
    Core.lookupObject (stRIS st) (stFilters st) ref
  streamContent s = do
    ris <- getRIS
    filters <- lift $ Pdf' $ gets stFilters
    Core.streamContent ris filters lookupM s
    where
    lookupM r = mapObject (const ()) `liftM` lookupObject r
  getRIS = lift $ Pdf' $ gets stRIS

-- | Execute PDF action with 'RIS'
runPdf :: MonadIO m => RIS -> [StreamFilter] -> Pdf m a -> m (Either PdfError a)
runPdf ris filters action = runPdf' ris filters $ runEitherT action

-- | Execute PDF action with 'Handle'
runPdfWithHandle :: MonadIO m => Handle -> [StreamFilter] -> Pdf m a -> m (Either PdfError a)
runPdfWithHandle handle filters action = do
  ris <- liftIO $ fromHandle handle
  runPdf ris filters action

runPdf' :: MonadIO m => RIS -> [StreamFilter] -> Pdf' m a -> m a
runPdf' ris filters (Pdf' action) = evalStateT action $ PdfState ris filters

-- | Get PDF document
document :: MonadIO m => Pdf m Document
document = do
  ris <- getRIS
  xref <- lastXRef ris
  tr <- trailer ris xref
  return $ Document tr
