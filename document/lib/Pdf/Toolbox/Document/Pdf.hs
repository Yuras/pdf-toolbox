{-# LANGUAGE OverloadedStrings #-}

module Pdf.Toolbox.Document.Pdf
(
  Pdf,
  pdfWithFile,
  pdfWithHandle,
  document,
  lookupObject,
  streamContent,
  rawStreamContent,
  decryptStream,
  decodeStream,
  deref,
  isEncrypted,
  setUserPassword,
)
where

import Data.Monoid
import Data.Int
import Data.IORef
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap
import Control.Applicative
import Control.Exception
import System.IO (Handle)
import System.IO.Streams (InputStream)

import Pdf.Toolbox.Core hiding (rawStreamContent, decodeStream)
import qualified Pdf.Toolbox.Core as Core

import Pdf.Toolbox.Document.File (File)
import qualified Pdf.Toolbox.Document.File as File
import Pdf.Toolbox.Document.Internal.Types
import Pdf.Toolbox.Document.Encryption

-- | Make Pdf with interface to pdf file
pdfWithFile :: File -> IO Pdf
pdfWithFile f = Pdf f <$> newIORef Nothing

-- | Make Pdf with seekable handle
pdfWithHandle :: Handle -> IO Pdf
pdfWithHandle h = do
  buf <- handleToBuffer h
  File.withBuffer knownFilters buf >>= pdfWithFile

file :: Pdf -> File
file (Pdf f _) = f

-- | Get PDF document
document :: Pdf -> IO Document
document pdf = Document pdf <$> File.trailer (file pdf)

-- | Find object by it's reference
lookupObject :: Pdf -> Ref -> IO (Object Int64)
lookupObject pdf ref = do
  (obj, decrypted) <- File.object (file pdf) ref
  if decrypted
    then return obj
    else decrypt pdf ref obj

-- | Get stream content, decoded and decrypted
--
-- Note: length of the content may differ from the raw one
streamContent :: Pdf
              -> Ref
              -> Stream Int64
              -> IO (Stream (InputStream ByteString))
streamContent pdf ref s =
  rawStreamContent pdf s
  >>= decryptStream pdf ref
  >>= decodeStream pdf

-- | Get stream content without decrypting or decoding it
rawStreamContent
  :: Pdf
  -> Stream Int64
  -> IO (Stream (InputStream ByteString))
rawStreamContent pdf s@(Stream dict _) =
  Stream dict <$> File.stream (file pdf) s

-- | Decrypt stream content
--
-- Note: length may change when decrypting. E.g. AESV2 encryption handler
-- stored initializing vector in the first 16 bytes of the stream, and we
-- strip them here.
decryptStream
  :: Pdf
  -> Ref
  -> Stream (InputStream ByteString)
  -> IO (Stream (InputStream ByteString))
decryptStream pdf ref (Stream dict is) = do
  maybe_decryptor <- readIORef decrRef
  case maybe_decryptor of
    Nothing -> return (Stream dict is)
    Just decryptor -> Stream dict <$> decryptor ref DecryptStream is
  where
  Pdf _ decrRef = pdf

-- | Decode stream content
--
-- It should be already decrypted
decodeStream
  :: Pdf
  -> Stream (InputStream ByteString)
  -> IO (Stream (InputStream ByteString))
decodeStream _pdf s@(Stream dict _) =
  Stream dict <$> Core.decodeStream knownFilters s

-- | Recursively load indirect object
deref :: Pdf -> Object a -> IO (Object ())
deref pdf (ORef ref) = do
  o <- lookupObject pdf ref
  deref pdf o
deref _ (ONumber n) = return (ONumber n)
deref _ (OName n) = return (OName n)
deref _ (OStr str) = return (OStr str)
deref _ (OBoolean b) = return (OBoolean b)
deref _ (ODict d) = return (ODict d)
deref _ (OArray a) = return (OArray a)
deref _ (OStream (Stream dict _)) = return (OStream (Stream dict ()))
deref _ ONull = return ONull

-- | Whether the PDF document it encrypted
isEncrypted :: Pdf -> IO Bool
isEncrypted pdf = message "isEncrypted" $ do
  tr <- File.trailer (file pdf)
  case HashMap.lookup "Encrypt" tr of
    Nothing -> return False
    Just _ -> return True

-- | Set the password to be user for decryption
--
-- Returns False when the password is wrong
setUserPassword :: Pdf -> ByteString -> IO Bool
setUserPassword pdf pass = message "setUserPassword" $ do
  tr <- File.trailer (file pdf)
  enc <-
    case HashMap.lookup "Encrypt" tr of
      Nothing -> throw (Unexpected "document is not encrypted" [])
      Just o -> do
        o' <- deref pdf o
        case o' of
          ODict d -> return d
          ONull -> throw (Corrupted "encryption encryption dict is null" [])
          _ -> throw (Corrupted "document Encrypt should be a dictionary" [])
  let either_res = mkStandardDecryptor tr enc
        (ByteString.take 32 $ pass `mappend` defaultUserPassword)
  case either_res of
    Left err -> throw $ Corrupted err []
    Right Nothing -> return False
    Right (Just decr) -> do
      let Pdf _ ref = pdf
      writeIORef ref (Just decr)
      File.setDecryptor (file pdf) decr
      return True

-- | Decrypt PDF object using user password is set
decrypt :: Pdf -> Ref -> Object a -> IO (Object a)
decrypt (Pdf _ decr_ref) ref o = do
  maybe_decr <- readIORef decr_ref
  case maybe_decr of
    Nothing -> return o
    Just decr -> decryptObject decr ref o
