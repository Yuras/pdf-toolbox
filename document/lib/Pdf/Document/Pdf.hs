{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Pdf.Document.Pdf
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
  EncryptedError (..),
  enableCache,
  disableCache,
)
where

import Pdf.Core hiding (rawStreamContent, decodeStream)
import qualified Pdf.Core as Core
import qualified Pdf.Core.IO.Buffer as Buffer

import Pdf.Document.File (File)
import qualified Pdf.Document.File as File
import Pdf.Document.Internal.Types
import Pdf.Document.Encryption

import Data.Typeable
import Data.IORef
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Exception hiding (throw)
import System.IO (Handle)
import System.IO.Streams (InputStream)

-- | Make Pdf with interface to pdf file
pdfWithFile :: File -> IO Pdf
pdfWithFile f = Pdf f
  <$> newIORef Nothing
  <*> newIORef (False, HashMap.empty)

-- | Make Pdf with seekable handle
pdfWithHandle :: Handle -> IO Pdf
pdfWithHandle h = do
  buf <- Buffer.fromHandle h
  File.withBuffer knownFilters buf >>= pdfWithFile

file :: Pdf -> File
file (Pdf f _ _) = f

-- | Get PDF document
document :: Pdf -> IO Document
document pdf = do
  let Pdf _ decrRef _ = pdf
  encrypted <- isEncrypted pdf
  when encrypted $ do
    maybe_decr <- readIORef decrRef
    case maybe_decr of
      Nothing -> throwIO $
        EncryptedError "File is encrypted, use 'setUserPassword'"
      Just _ -> return ()

  Document pdf <$> File.trailer (file pdf)

-- | Find object by it's reference
lookupObject :: Pdf -> Ref -> IO Object
lookupObject pdf ref = do
  let Pdf _ _ cacheRef = pdf
  (useCache, cache) <- readIORef cacheRef
  case HashMap.lookup ref cache of
    Just obj -> return obj
    Nothing -> do
      (obj, decrypted) <- File.object (file pdf) ref
      obj' <- if decrypted
        then return obj
        else decrypt pdf ref obj
      when useCache $
        writeIORef cacheRef (useCache, HashMap.insert ref obj' cache)
      return obj'

-- | Cache object for future lookups
enableCache :: Pdf -> IO ()
enableCache (Pdf _ _ cacheRef) = do
  (_, cache) <- readIORef cacheRef
  writeIORef cacheRef (True, cache)

-- | Don't cache object for future lookups
disableCache :: Pdf -> IO ()
disableCache (Pdf _ _ cacheRef) = do
  (_, cache) <- readIORef cacheRef
  writeIORef cacheRef (False, cache)

-- | Get stream content, decoded and decrypted
--
-- Note: length of the content may differ from the raw one
streamContent :: Pdf
              -> Ref
              -> Stream
              -> IO (InputStream ByteString)
streamContent pdf ref s =
  rawStreamContent pdf s
  >>= decryptStream pdf ref
  >>= decodeStream pdf s

-- | Get stream content without decrypting or decoding it
rawStreamContent
  :: Pdf
  -> Stream
  -> IO (InputStream ByteString)
rawStreamContent pdf s =
  File.stream (file pdf) s

-- | Decrypt stream content
--
-- Note: length may change when decrypting. E.g. AESV2 encryption handler
-- stored initializing vector in the first 16 bytes of the stream, and we
-- strip them here.
decryptStream
  :: Pdf
  -> Ref
  -> InputStream ByteString
  -> IO (InputStream ByteString)
decryptStream pdf ref is = do
  maybe_decryptor <- readIORef decrRef
  case maybe_decryptor of
    Nothing -> return is
    Just decryptor -> decryptor ref DecryptStream is
  where
  Pdf _ decrRef _ = pdf

-- | Decode stream content
--
-- It should be already decrypted
decodeStream
  :: Pdf
  -> Stream -> InputStream ByteString
  -> IO (InputStream ByteString)
decodeStream _pdf s =
  Core.decodeStream knownFilters s

-- | Recursively load indirect object
deref :: Pdf -> Object -> IO Object
deref pdf (Ref ref) = do
  o <- lookupObject pdf ref
  deref pdf o
deref _ o = return o

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
      Nothing -> throwIO (Unexpected "document is not encrypted" [])
      Just o -> do
        o' <- deref pdf o
        case o' of
          Dict d -> return d
          Null -> throwIO (Corrupted "encryption encryption dict is null" [])
          _ -> throwIO (Corrupted "document Encrypt should be a dictionary" [])
  let either_res = mkStandardDecryptor tr enc
        (ByteString.take 32 $ pass `mappend` defaultUserPassword)
  case either_res of
    Left err -> throwIO $ Corrupted err []
    Right Nothing -> return False
    Right (Just decr) -> do
      let Pdf _ ref _ = pdf
      writeIORef ref (Just decr)
      File.setDecryptor (file pdf) decr
      return True

-- | Decrypt PDF object using user password is set
decrypt :: Pdf -> Ref -> Object -> IO Object
decrypt (Pdf _ decr_ref _) ref o = do
  maybe_decr <- readIORef decr_ref
  case maybe_decr of
    Nothing -> return o
    Just decr -> decryptObject decr ref o

-- | File is enctypted
data EncryptedError = EncryptedError Text
  deriving (Show, Typeable)

instance Exception EncryptedError
