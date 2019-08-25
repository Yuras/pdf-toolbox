{-# LANGUAGE OverloadedStrings #-}

-- | Pdf file as a set of objects

module Pdf.Core.File
( File(..)
, withPdfFile
, fromHandle
, fromBuffer
, lastTrailer
, findObject
, streamContent
, rawStreamContent
, EncryptionStatus(..)
, encryptionStatus
, setUserPassword
, setDecryptor
, NotFound(..)
)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Exception (Exception, throwIO, catch)
import System.IO (Handle)
import qualified System.IO as IO
import System.IO.Streams (InputStream)

import Pdf.Core.Object
import Pdf.Core.Object.Util
import Pdf.Core.Exception
import Pdf.Core.XRef
import Pdf.Core.Stream (StreamFilter)
import Pdf.Core.Util
import qualified Pdf.Core.Stream as Stream
import Pdf.Core.IO.Buffer (Buffer)
import qualified Pdf.Core.IO.Buffer as Buffer
import Pdf.Core.Encryption

-- | Pdf file is a collection of 'Object's
data File = File
  { fileLastXRef :: XRef
  , fileBuffer :: Buffer
  , fileFilters :: [StreamFilter]
  , fileDecryptor :: IORef (Maybe Decryptor)
  }

-- | The last trailer is an entry point to PDF file. All other objects
-- usually are referensed from it, directly or indirectly.
lastTrailer :: File -> IO Dict
lastTrailer file = trailer (fileBuffer file) (fileLastXRef file)

-- | Get an object with the specified ref.
findObject :: File -> Ref -> IO Object
findObject file ref = do
  mentry <- fmap Just (lookupEntryRec file ref)
    `catch` \(UnknownXRefStreamEntryType _) -> return Nothing
  case mentry of
    Nothing -> return Null
    Just entry -> readObjectForEntry file entry

-- | Get content of the stream
--
-- It's decrypted and decoded using registered 'StreamFilter's if necessary.
streamContent :: File -> Ref -> Stream -> IO (InputStream ByteString)
streamContent file ref s = do
  is <- rawStreamContent file ref s
  Stream.decodeStream (fileFilters file) s is

-- | Get content of the stream
--
-- Content would be decrypted if necessary.
rawStreamContent :: File -> Ref -> Stream -> IO (InputStream ByteString)
rawStreamContent file ref (S dict pos) = do
  len <- do
    obj <- sure $ HashMap.lookup "Length" dict
      `notice` "Length missing in stream"
    case obj of
      Number _ -> sure $ intValue obj
        `notice` "Length should be an integer"
      Ref r -> do
        o <- findObject file r
        sure $ intValue o `notice` "Length should be an integer"
      _ -> throwIO $ Corrupted "Length should be an integer" []
  is <- Stream.rawStreamContent (fileBuffer file) len pos
  mdecryptor <- readIORef (fileDecryptor file)
  case mdecryptor of
    Nothing -> return is
    Just decryptor -> decryptor ref DecryptStream is

-- | Describes wether PDF file is encrypted, plain or already decrypted
data EncryptionStatus
  = Encrypted  -- ^ requires decryption
  | Decrypted  -- ^ already decrypted
  | Plain      -- ^ doesn't require decryption
  deriving (Show, Eq, Enum)

-- | Get encryption status.
--
-- If it's 'Encrypted', you may want to 'setUserPassword' to decrypt it.
encryptionStatus :: File -> IO EncryptionStatus
encryptionStatus file = do
  tr <- lastTrailer file
  case HashMap.lookup "Encrypt" tr of
    Nothing -> return Plain
    Just _ -> do
      decr <- readIORef (fileDecryptor file)
      case decr of
        Nothing -> return Encrypted
        Just _ -> return Decrypted

-- | Set user password to decrypt PDF file.
--
-- Use empty bytestring to set the default password.
-- Returns @True@ on success.
-- See also 'setDecryptor'.
setUserPassword :: File -> ByteString -> IO Bool
setUserPassword file password = message "setUserPassword" $ do
  tr <- lastTrailer file
  enc <-
    case HashMap.lookup "Encrypt" tr of
      Nothing -> throwIO (Unexpected "document is not encrypted" [])
      Just o -> do
        o' <- deref file o
        case o' of
          Dict d -> return d
          Null -> throwIO (Corrupted "encryption encryption dict is null" [])
          _ -> throwIO (Corrupted "document Encrypt should be a dictionary" [])
  let either_decryptor = mkStandardDecryptor tr enc
        (ByteString.take 32 $ password `mappend` defaultUserPassword)
  case either_decryptor of
    Left err -> throwIO $ Corrupted err []
    Right Nothing -> return False
    Right (Just decryptor) -> do
      setDecryptor file decryptor
      return True
  where
  deref f (Ref ref) = findObject f ref
  deref _ o = return o

-- | Decrypt file using the specified decryptor.
--
-- Use it if 'setUserPassword' doesn't work for you.
setDecryptor :: File -> Decryptor -> IO ()
setDecryptor file decryptor =
  writeIORef (fileDecryptor file) (Just decryptor)

-- | Create file from a buffer.
--
-- You may use 'Stream.knownFilters' as the first argument.
fromBuffer :: [StreamFilter] -> Buffer -> IO File
fromBuffer filters buffer = do
  xref <- lastXRef buffer
  decryptor <- newIORef Nothing
  return File
    { fileLastXRef = xref
    , fileBuffer = buffer
    , fileFilters = filters
    , fileDecryptor = decryptor
    }

-- | Create file from a binary handle.
--
-- You may use 'Stream.knownFilters' as the first argument.
fromHandle :: [StreamFilter] -> Handle -> IO File
fromHandle filters handle = do
  buffer <- Buffer.fromHandle handle
  fromBuffer filters buffer

-- | Open Pdf file
--
-- You may want to check 'encryptionStatus' and 'setUserPassword' if
-- file is encrypted.
withPdfFile :: FilePath -> (File -> IO a) -> IO a
withPdfFile path action =
  IO.withBinaryFile path IO.ReadMode $ \handle -> do
    file <- fromHandle Stream.knownFilters handle
    action file

lookupEntryRec :: File -> Ref -> IO Entry
lookupEntryRec file ref = loop (fileLastXRef file)
  where
  loop xref = do
    res <- lookupEntry file ref xref
    case res of
      Just e -> return e
      Nothing -> do
        prev <- prevXRef (fileBuffer file) xref
        case prev of
          Just p -> loop p
          Nothing -> throwIO (NotFound $ "The Ref not found: " ++ show ref)

lookupEntry :: File -> Ref -> XRef -> IO (Maybe Entry)
lookupEntry file ref xref@(XRefTable _) =
  lookupTableEntry (fileBuffer file) xref ref
lookupEntry file ref (XRefStream _ s@(S dict _)) = do
  content <- streamContent file ref s
  lookupStreamEntry dict content ref

readObjectForEntry :: File -> Entry -> IO Object

readObjectForEntry _ EntryFree{} = return Null

readObjectForEntry file (EntryUsed off gen) = do
  (ref, obj) <- readObjectAtOffset (fileBuffer file) off
  let R _ gen' = ref
  unless (gen' == gen) $
    throwIO (Corrupted "readObjectForEntry" ["object generation missmatch"])
  decrypt file ref obj

readObjectForEntry file (EntryCompressed index num) = do
  let ref= R index 0
  objStream@(S dict _) <- do
    o <- findObject file ref
    sure $ streamValue o `notice` "Compressed entry should be in stream"
  first <- sure $ (HashMap.lookup "First" dict >>= intValue)
      `notice` "First should be an integer"
  content <- streamContent file ref objStream
  readCompressedObject content (fromIntegral first) num

decrypt :: File -> Ref -> Object -> IO Object
decrypt file ref o = do
  maybe_decr <- readIORef (fileDecryptor file)
  case maybe_decr of
    Nothing -> return o
    Just decr -> decryptObject decr ref o

data NotFound = NotFound String
  deriving (Show)

instance Exception NotFound
