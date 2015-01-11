{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- PDF file as container for objects

module Pdf.Toolbox.Document.File
(
  File(..),
  NotFound(..),
  withBuffer
)
where

import Data.Int
import Data.Typeable
import Data.ByteString (ByteString)
import Control.Applicative
import Control.Monad
import Control.Exception
import System.IO.Streams (InputStream)

import Pdf.Toolbox.Core hiding (trailer)
import qualified Pdf.Toolbox.Core as Core
import Pdf.Toolbox.Core.Util

-- | PDF file
--
-- It doesn't perform decryption or decoding
data File = File
  { object :: Ref -> IO (Object Int64)
  , stream :: Stream Int64 -> IO (InputStream ByteString)
  , trailer :: IO Dict
  }

data File_ = File_
  { _lastXRef :: XRef
  , _buffer :: Buffer
  , _filters :: [StreamFilter]
  }

-- | PDF file from buffer
withBuffer :: [StreamFilter] -> Buffer -> IO File
withBuffer filters buf = do
  xref <- lastXRef buf
  let file = File_
        { _lastXRef = xref
        , _buffer = buf
        , _filters = filters
        }
  return File
    { object = findObject file
    , stream = streamContent file
    , trailer = Core.trailer buf xref
    }

findObject :: File_ -> Ref -> IO (Object Int64)
findObject file ref =
  (lookupEntryRec file ref
  >>= readObjectForEntry file)
    -- unknown type should be interpreted as reference to null object
    `catch` \(UnknownXRefStreamEntryType _) -> return ONull

streamContent :: File_ -> Stream Int64 -> IO (InputStream ByteString)
streamContent file s@(Stream dict _) = do
  len <- do
    obj <- sure $ lookupDict "Length" dict `notice` "Length missing in stream"
    case obj of
      ONumber _ -> sure $ intValue obj `notice` "Length should be an integer"
      ORef ref -> do
        o <- findObject file ref
        sure $ intValue o `notice` "Length should be an integer"
      _ -> throw $ Corrupted "Length should be an integer" []
  rawStreamContent (_buffer file) len s

readObjectForEntry :: File_-> XRefEntry -> IO (Object Int64)
readObjectForEntry file (XRefTableEntry entry)
  | teIsFree entry = return ONull
  | otherwise = do
    (Ref _ gen, obj) <- readObjectAtOffset (_buffer file) (teOffset entry)
    unless (gen == teGen entry) $
      throw (Corrupted "readObjectForEntry" ["object generation missmatch"])
    return obj
readObjectForEntry file (XRefStreamEntry entry) =
  case entry of
    StreamEntryFree{} -> return ONull
    StreamEntryUsed off _ ->
      snd <$> readObjectAtOffset (_buffer file) off
    StreamEntryCompressed index num -> do
      objStream@(Stream dict _) <- do
        o <- findObject file (Ref index 0)
        sure $ streamValue o `notice` "Compressed entry should be in stream"
      first <- sure $ (lookupDict "First" dict >>= intValue)
          `notice` "First should be an integer"
      raw <- streamContent file objStream
      decoded <- decodeStream (_filters file) (Stream dict raw)
      conv <$> readCompressedObject decoded (fromIntegral first) num
  where
  conv (OStr v) = OStr v
  conv (OName v) = OName v
  conv (ONumber v) = ONumber v
  conv (OArray v) = OArray v
  conv (ODict v) = ODict v
  conv (OBoolean v) = OBoolean v
  conv (ORef v) = ORef v
  conv ONull = ONull
  conv OStream{} = error "conv: impossible: stream"

lookupEntryRec :: File_ -> Ref -> IO XRefEntry
lookupEntryRec file ref = loop (_lastXRef file)
  where
  loop xref = do
    res <- lookupEntry file ref xref
    case res of
      Just e -> return e
      Nothing -> do
        prev <- prevXRef (_buffer file) xref
        case prev of
          Just p -> loop p
          Nothing -> throw (NotFound $ "The Ref not found: " ++ show ref)

lookupEntry :: File_ -> Ref -> XRef -> IO (Maybe XRefEntry)
lookupEntry file ref xref@(XRefTable _) =
  fmap XRefTableEntry <$> lookupTableEntry (_buffer file) xref ref
lookupEntry file ref (XRefStream _ s@(Stream dict _)) = do
  raw <- streamContent file s
  decoded <- decodeStream (_filters file) (Stream dict raw)
  fmap XRefStreamEntry <$> lookupStreamEntry (Stream dict decoded) ref

data NotFound = NotFound String
  deriving (Show, Typeable)

instance Exception NotFound
