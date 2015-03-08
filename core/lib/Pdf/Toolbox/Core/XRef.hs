{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Cross reference

module Pdf.Toolbox.Core.XRef
(
  XRef(..),
  XRefEntry(..),
  TableEntry(..),
  StreamEntry(..),
  readXRef,
  lastXRef,
  prevXRef,
  trailer,
  lookupTableEntry,
  lookupStreamEntry,
  isTable,
  UnknownXRefStreamEntryType(..),
)
where

import Data.Typeable
import Data.Int
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Applicative
import Control.Monad
import Control.Exception
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Object.Util
import Pdf.Toolbox.Core.Parsers.XRef
import Pdf.Toolbox.Core.Stream
import Pdf.Toolbox.Core.IO.Buffer
import Pdf.Toolbox.Core.Exception
import Pdf.Toolbox.Core.Util

-- | Entry in cross reference table
data TableEntry = TableEntry {
  teOffset :: Int64,
  teGen :: Int,
  teIsFree :: Bool
  } deriving (Eq, Show)

-- | Entry in cross reference stream
data StreamEntry =
  -- | Object number and generation
  StreamEntryFree Int Int |
  -- | Object offset (in bytes from the beginning of file) and generation
  StreamEntryUsed Int64 Int |
  -- | Object number of object stream and index within the object stream
  StreamEntryCompressed Int Int
  deriving (Eq, Show)

-- | Entry in cross reference
data XRefEntry =
  XRefTableEntry TableEntry |
  XRefStreamEntry StreamEntry
  deriving Show

-- | Cross reference
data XRef =
  -- | Offset
  XRefTable Int64 |
  -- | Offset and stream with content offset
  XRefStream Int64 (Stream Int64)
  deriving (Eq, Show)

-- | Check whether the stream starts with \"xref\" keyword.
-- The keyword itself and newline after it are consumed
isTable :: InputStream ByteString -> IO Bool
isTable is = (Streams.parseFromStream tableXRef is >> return True)
  `catch` \(Streams.ParseException _) -> return False

-- | Find the last cross reference
lastXRef :: Buffer -> IO XRef
lastXRef buf = do
  sz <- bufferSize buf
  bufferSeek buf $ max 0 (sz - 1024)
  (Streams.parseFromStream startXRef (bufferToInputStream buf)
    >>= readXRef buf
    ) `catch` \(Streams.ParseException msg) ->
                  throw (Corrupted "lastXRef" [msg])

-- | Read XRef at specified offset
readXRef :: Buffer -> Int64 -> IO XRef
readXRef buf off = do
  bufferSeek buf off
  let is = bufferToInputStream buf
  table <- isTable is
  if table
    then return (XRefTable off)
    else XRefStream off <$> readStream is off

-- | Find prev cross reference
prevXRef :: Buffer -> XRef -> IO (Maybe XRef)
prevXRef buf xref = message "prevXRef" $ do
  tr <- trailer buf xref
  case HashMap.lookup "Prev" tr of
    Just prev -> do
      off <- sure $ intValue prev
        `notice` "Prev in trailer should be an integer"
      Just <$> readXRef buf (fromIntegral off)
    _ -> return Nothing

-- | Read trailer for the xref
trailer :: Buffer -> XRef -> IO Dict
trailer buf (XRefTable off) = do
  bufferSeek buf off
  let is = bufferToInputStream buf
  table <- isTable is
  unless table $
    throw (Unexpected "trailer" ["table not found"])
  ( skipTable is >>
    Streams.parseFromStream parseTrailerAfterTable is
    ) `catch` \(Streams.ParseException msg) ->
                  throw (Corrupted "trailer" [msg])
trailer _ (XRefStream _ (Stream dict _)) = return dict

skipTable :: InputStream ByteString -> IO ()
skipTable is = message "skipTable" $
  (subsectionHeader is
    `catch` \(Streams.ParseException msg) ->
      throw (Corrupted msg []))
    >>= go . snd
  where
  go count = nextSubsectionHeader is count >>= maybe (return ()) (go . snd)

subsectionHeader :: InputStream ByteString -> IO (Int, Int)
subsectionHeader = Streams.parseFromStream parseSubsectionHeader

nextSubsectionHeader :: InputStream ByteString -> Int -> IO (Maybe (Int, Int))
nextSubsectionHeader is count = message "nextSubsectionHeader" $ do
  skipSubsection is count
  fmap Just (subsectionHeader is)
    `catch` \(Streams.ParseException _) -> return Nothing

skipSubsection :: InputStream ByteString -> Int -> IO ()
skipSubsection is count = dropExactly (count * 20) is

-- | Read xref entry for the indirect object from xref table
lookupTableEntry :: Buffer
                 -> XRef  -- ^ should be xref table
                 -> Ref   -- ^ indirect object to look for
                 -> IO (Maybe TableEntry)
lookupTableEntry buf (XRefTable tableOff) (R index gen)
  = message "lookupTableEntry" $ do
  bufferSeek buf tableOff
  table <- isTable (bufferToInputStream buf)
  unless table $
    throw $ Unexpected "Not a table" []
  (subsectionHeader (bufferToInputStream buf) >>= go)
    `catch` \(Streams.ParseException err) -> throw (Corrupted err [])
  where
  go (start, count) = do
    if index >= start && index < start + count
      then do
        -- that is our section, lets seek to the row
        bufferTell buf
          >>= bufferSeek buf . (+ (fromIntegral $ index - start) * 20)
        (off, gen', free) <-
          Streams.parseFromStream parseTableEntry (bufferToInputStream buf)
            `catch` \(Streams.ParseException msg) ->
              throw (Corrupted "parseTableEntry failed" [msg])
        unless (gen == gen') $
          throw $ Corrupted "Generation mismatch" []
        return $ Just $ TableEntry off gen free
      else
        -- go to the next section if any
        nextSubsectionHeader (bufferToInputStream buf) count
        >>= maybe (return Nothing) go
lookupTableEntry _ XRefStream{} _ =
  throw $ Unexpected "lookupTableEntry" ["Only xref table allowed"]

-- | Read xref entry for the indirect object from xref stream
--
-- See pdf1.7 spec: 7.5.8 Cross-Reference Streams.
-- May throw 'UnknownXRefStreamEntryType'
lookupStreamEntry
  :: Stream (InputStream ByteString)  -- ^ decoded xref stream content
  -> Ref                              -- ^ indirect object
  -> IO (Maybe StreamEntry)
lookupStreamEntry (Stream dict is) (R objNumber _) =
  message "lookupStreamEntry" $ do

  index <- sure $ do
    sz <- (HashMap.lookup "Size" dict >>= intValue)
      `notice` "Size should be an integer"
    i <-
      case HashMap.lookup "Index" dict of
        Nothing           -> Right [ONumber 0, ONumber (fromIntegral sz)]
        Just (OArray arr) -> Right (Vector.toList arr)
        _                 -> Left "Index should be an array"

    let convertIndex res [] = Right (reverse res)
        convertIndex res (x1:x2:xs) = do
          from <- intValue x1 `notice` "from index should be an integer"
          count <- intValue x2 `notice` "count should be an integer"
          convertIndex ((from, count) : res) xs
        convertIndex _ _ = Left $ "Malformed Index in xref stream: " ++ show i

    convertIndex [] i

  width <- sure $ do
    ws <-
      case HashMap.lookup "W" dict of
        Just (OArray ws) -> Right (Vector.toList ws)
        _ -> Left "W should be an array"
    mapM intValue ws
      `notice` "W should contains integers"

  unless (length width == 3) $
    throw $ Corrupted ("Malformed With array in xref stream: "
                        ++ show width) []

  values <- do
    let position = loop 0 index
        totalWidth = sum width
        loop _ [] = Nothing
        loop pos ((from, count) : xs) =
          if objNumber < from || objNumber >= from + count
            then loop (pos + totalWidth * count) xs
            else Just (pos + totalWidth * (objNumber - from))
    case position of
      Nothing -> return Nothing
      Just p -> do
        dropExactly p is
        Just . ByteString.unpack <$> Streams.readExactly totalWidth is

  case values of
    Nothing -> return Nothing
    Just vs -> do
      let [v1, v2, v3] = map conv $ collect [] width vs :: [Int64]
            where
            conv l = conv' (length l - 1) 0 l
            conv' _ res [] = res
            conv' power res (x:xs) =
              conv' (power-1) (res + (fromIntegral x * 256 ^ power)) xs
            collect res [] [] = reverse res
            collect res (x:xs) ys = collect (take x ys : res) xs (drop x ys)
            collect _ _ _ = error "readStreamEntry: collect: impossible"
      case v1 of
        0 -> return $ Just $ StreamEntryFree (fromIntegral v2)
                                             (fromIntegral v3)
        1 -> return $ Just $ StreamEntryUsed v2 (fromIntegral v3)
        2 -> return $ Just $ StreamEntryCompressed (fromIntegral v2)
                                                   (fromIntegral v3)
        _ -> throw $ UnknownXRefStreamEntryType (fromIntegral v1)

-- | Unknown entry type should be interpreted as reference to null object
data UnknownXRefStreamEntryType = UnknownXRefStreamEntryType Int
  deriving (Show, Typeable)

instance Exception UnknownXRefStreamEntryType
