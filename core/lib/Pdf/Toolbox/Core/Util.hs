
-- | Unclassified tools

module Pdf.Toolbox.Core.Util
(
  readObjectAtOffset
)
where

import Data.Int
import Control.Monad

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Parsers.Object
import Pdf.Toolbox.Core.Error
import Pdf.Toolbox.Core.IO

-- | Read indirect object at the specified offset
readObjectAtOffset :: MonadIO m
                   => RIS              -- ^ input stream to read from
                   -> Int64            -- ^ object offset
                   -> Int              -- ^ object generation
                   -> PdfE m (Object Int64)
readObjectAtOffset ris off gen = do
  seek ris off
  (Ref _ gen', o) <- inputStream ris >>= parse parseIndirectObject
  unless (gen == gen') $ left $ UnexpectedError $ "Generation mismatch, expected: " ++ show gen ++ ", found: " ++ show gen'
  case o of
    ONumber val -> return $ ONumber val
    OBoolean val -> return $ OBoolean val
    OName val -> return $ OName val
    ODict val -> return $ ODict val
    OArray val -> return $ OArray val
    OStr val -> return $ OStr val
    OStream (Stream dict _) -> (OStream . Stream dict) `liftM` tell ris
    ORef _ -> left $ UnexpectedError "Indirect object can't be ORef"
    ONull -> return ONull
