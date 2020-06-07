{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Pdf.Document.Pdf
(
  Pdf,
  withPdfFile,
  fromFile,
  fromHandle,
  fromBytes,
  document,
  lookupObject,
  streamContent,
  rawStreamContent,
  deref,
  isEncrypted,
  setUserPassword,
  defaultUserPassword,
  EncryptedError (..),
  enableCache,
  disableCache,
)
where

import Pdf.Core.Object
import Pdf.Core.Stream (knownFilters)
import Pdf.Core.File (File)
import qualified Pdf.Core.File as File
import Pdf.Core.Encryption (defaultUserPassword)

import Pdf.Document.Internal.Types

import Data.Typeable
import Data.IORef
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Exception hiding (throw)
import System.IO (Handle)
import System.IO.Streams (InputStream)

withPdfFile :: FilePath -> (Pdf -> IO a) -> IO a
withPdfFile path action = File.withPdfFile path $ \f -> do
  pdf <- fromFile f
  action pdf

-- | Make Pdf with interface to pdf file
fromFile :: File -> IO Pdf
fromFile f = Pdf f
  <$> newIORef (False, HashMap.empty)

-- | Make Pdf with seekable handle
fromHandle :: Handle -> IO Pdf
fromHandle h = do
  File.fromHandle knownFilters h >>= fromFile

-- | Make Pdf from a ByteString
fromBytes :: ByteString -> IO Pdf
fromBytes h = do
  File.fromBytes knownFilters h >>= fromFile

file :: Pdf -> File
file (Pdf f _) = f

-- | Get PDF document
document :: Pdf -> IO Document
document pdf = do
  status <- File.encryptionStatus (file pdf)
  case status of
    File.Encrypted -> throwIO $
      EncryptedError "File is encrypted, use 'setUserPassword'"
    File.Decrypted -> return ()
    File.Plain -> return ()

  Document pdf <$> File.lastTrailer (file pdf)

-- | Find object by it's reference
lookupObject :: Pdf -> Ref -> IO Object
lookupObject pdf ref = do
  let Pdf _ cacheRef = pdf
  (useCache, cache) <- readIORef cacheRef
  case HashMap.lookup ref cache of
    Just obj -> return obj
    Nothing -> do
      obj <- File.findObject (file pdf) ref
      when useCache $
        writeIORef cacheRef (useCache, HashMap.insert ref obj cache)
      return obj

-- | Cache object for future lookups
enableCache :: Pdf -> IO ()
enableCache (Pdf _ cacheRef) = do
  (_, cache) <- readIORef cacheRef
  writeIORef cacheRef (True, cache)

-- | Don't cache object for future lookups
disableCache :: Pdf -> IO ()
disableCache (Pdf _ cacheRef) = do
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
  File.streamContent (file pdf) ref s

-- | Get stream content without decoding it
rawStreamContent
  :: Pdf
  -> Ref
  -> Stream
  -> IO (InputStream ByteString)
rawStreamContent pdf ref s =
  File.rawStreamContent (file pdf) ref s

-- | Whether the PDF document it encrypted
isEncrypted :: Pdf -> IO Bool
isEncrypted pdf = do
  status <- File.encryptionStatus (file pdf)
  return $ case status of
    File.Encrypted -> True
    File.Decrypted -> True
    File.Plain -> False

-- | Set the password to be user for decryption
--
-- Returns False when the password is wrong
setUserPassword :: Pdf -> ByteString -> IO Bool
setUserPassword pdf password =
  File.setUserPassword (file pdf) password

deref :: Pdf -> Object -> IO Object
deref pdf (Ref r) = lookupObject pdf r
deref _ o = return o

-- | File is enctypted
data EncryptedError = EncryptedError Text
  deriving (Show, Typeable)

instance Exception EncryptedError
