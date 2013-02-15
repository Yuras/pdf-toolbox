
-- | Unclassified tools

module Pdf.Toolbox.Core.Util
(
  readObjectAtOffset,
  readCompressedObject
)
where

import Data.Int
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import Control.Monad
import qualified System.IO.Streams as Streams

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

-- | Read object from object stream
readCompressedObject :: MonadIO m
                     => IS         -- ^ input object stream decoded content
                     -> Int64      -- ^ an offset of the first object (\"First\" key in dictionary)
                     -> Int        -- ^ object number to read
                     -> PdfE m (Object ())
readCompressedObject is first num = do
  (is', countConsumed) <- liftIO $ Streams.countInput is
  res <- replicateM (num + 1) $ parse headerP is' :: MonadIO m => PdfE m [(Int, Int64)]
  (_, off) <- tryLast (UnexpectedError $ "readCompressedObject: tryLast: impossible") res
  pos <- liftIO $ countConsumed
  dropExactly (fromIntegral $ first + off - pos) is
  parse parseObject is
  where
  headerP = do
    n <- Parser.decimal
    Parser.skipSpace
    off <- Parser.decimal
    Parser.skipSpace
    return (n, off)
