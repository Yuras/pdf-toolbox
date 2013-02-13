
-- | Interface to the underlying PDF file

module Pdf.Toolbox.Document.Monad
(
  MonadPdf(..),
  deref
)
where

import Data.Int

import Pdf.Toolbox.Core hiding (lookupObject)

-- | Interface to the underlying PDF file
class Monad m => MonadPdf m where
  -- | find object by it's reference
  lookupObject :: Ref -> PdfE m (Object Int64)
  -- | decoded stream content
  streamContent :: Stream Int64 -> PdfE m (Stream IS)
  -- | underlying 'RIS'
  getRIS :: PdfE m RIS

-- | Recursively load indirect object
deref :: (MonadPdf m, Show a) => Object a -> PdfE m (Object ())
deref (ORef ref) = do
  o <- lookupObject ref
  deref o
deref o =
  case o of
    ONumber n -> return $ ONumber n
    OBoolean b -> return $ OBoolean b
    OName name -> return $ OName name
    ODict dict -> return $ ODict dict
    OArray array -> return $ OArray array
    OStr str -> return $ OStr str
    OStream _ -> left $ UnexpectedError $ "deref: found steam for object: " ++ show o
    ORef _ -> left $ UnexpectedError $ "deref: found ref for object: " ++ show o
    ONull -> return ONull
