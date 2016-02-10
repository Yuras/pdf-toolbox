{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Write PDF files
--
-- It could be used to generate new PDF file
-- or to incrementally update the existent one
--
-- To generate new file, first call 'writeHeader',
-- then a number of 'writeObject' and finally 'writeXRefTable'
--
-- To incrementally update PDF file just omit the
-- `writeHeader` and append the result to the existent file

module Pdf.Core.Writer
( Writer
, makeWriter
, writeHeader
, writeObject
, writeStream
, deleteObject
, writeXRefTable
)
where

import Pdf.Core.Object.Types
import Pdf.Core.Object.Builder

import Data.IORef
import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Builder
import Data.Function
import Control.Monad
import System.IO.Streams (OutputStream)
import qualified System.IO.Streams as Streams

newtype Writer = Writer {toStateRef :: IORef State}

makeWriter :: OutputStream ByteString -> IO Writer
makeWriter output = do
  (out, count) <- Streams.countOutput output
  let emptyState = State {
        stOutput = out,
        stObjects = Set.empty,
        stCount = count,
        stOffset = 0
        }
  Writer <$> newIORef emptyState

data Elem = Elem {
  elemIndex :: {-# UNPACK #-} !Int,
  elemGen :: {-# UNPACK #-} !Int,
  elemOffset :: {-# UNPACK #-} !Int64,
  elemFree :: !Bool
  }

instance Eq Elem where
  (==) = (==) `on` elemIndex

instance Ord Elem where
  compare = compare `on` elemIndex

data State = State {
  stOutput :: OutputStream ByteString,
  stObjects :: !(Set Elem),
  stCount :: IO Int64,
  stOffset :: {-# UNPACK #-} !Int64
  }

-- | Write PDF header. Used for generating new PDF files.
-- Should be the first call. Not used fo incremental updates
writeHeader :: Writer -> IO ()
writeHeader writer = do
  st <- readIORef (toStateRef writer)
  Streams.write (Just "%PDF-1.7\n") (stOutput st)

-- | Write object
writeObject :: Writer -> Ref -> Object -> IO ()
writeObject writer ref@(R index gen) obj = do
  pos <- countWritten writer
  st <- readIORef (toStateRef writer)
  addElem writer $ Elem index gen pos False
  dumpObject (stOutput st) ref obj

-- | Write stream
writeStream :: Writer -> Ref -> Dict -> BSL.ByteString -> IO ()
writeStream writer ref@(R index gen) dict dat = do
  pos <- countWritten writer
  st <- readIORef (toStateRef writer)
  addElem writer $ Elem index gen pos False
  dumpStream (stOutput st) ref dict dat

-- | Delete object
deleteObject :: Writer -> Ref -> Int64 -> IO ()
deleteObject writer (R index gen) nextFree =
  addElem writer $ Elem index gen nextFree True

-- | Write xref table. Should be the last call.
-- Used for generating and incremental updates.
writeXRefTable
  :: Writer
  -> Int64    -- ^ size of the original PDF file. Should be 0 for new file
  -> Dict     -- ^ trailer
  -> IO ()
writeXRefTable writer offset tr = do
  off <- (+ offset) <$> countWritten writer
  st <- readIORef (toStateRef writer)
  let elems = Set.mapMonotonic (\e -> e {elemOffset = elemOffset e + offset})
            $ stObjects st
      content = mconcat
        [ byteString "xref\n"
        , buildXRefTable (Set.toAscList elems)
        , byteString "trailer\n"
        , buildDict tr
        , byteString "\nstartxref\n"
        , int64Dec off
        , byteString "\n%%EOF\n"
        ]
  Streams.writeLazyByteString (toLazyByteString content) (stOutput st)

countWritten :: Writer -> IO Int64
countWritten writer = do
  st <- readIORef (toStateRef writer)
  c <- (stOffset st +) <$> stCount st
  writeIORef (toStateRef writer) st{stOffset = c}
  return $! c

addElem :: Writer -> Elem -> IO ()
addElem writer e = do
  st <- readIORef (toStateRef writer)
  when (Set.member e $ stObjects st) $
    error $ "Writer: attempt to write object with the same index: " ++ show (elemIndex e)
  writeIORef (toStateRef writer) $ st
    { stObjects = Set.insert e $ stObjects st
    }

dumpObject :: OutputStream ByteString -> Ref -> Object -> IO ()
dumpObject out ref o =
  Streams.writeLazyByteString
    (toLazyByteString $ buildIndirectObject ref o)
    out

dumpStream :: OutputStream ByteString -> Ref -> Dict -> BSL.ByteString -> IO ()
dumpStream out ref dict dat =
  Streams.writeLazyByteString
    (toLazyByteString $ buildIndirectStream ref dict dat) out

buildXRefTable :: [Elem] -> Builder
buildXRefTable entries =
  mconcat (map buildXRefSection $ sections entries)
  where
  sections :: [Elem] -> [[Elem]]
  sections [] = []
  sections xs = let (s, rest) = section xs in s : sections rest
  section [] = error "impossible"
  section (x:xs) = go (elemIndex x + 1) [x] xs
    where
    go _ res [] = (reverse res, [])
    go i res (y:ys) =
      if i == elemIndex y
        then go (i + 1) (y : res) ys
        else (reverse res, y:ys)

buildXRefSection :: [Elem] -> Builder
buildXRefSection [] = error "impossible"
buildXRefSection s@(e:_) = mconcat
  [ intDec (elemIndex e)
  , char7 ' '
  , intDec (length s)
  , char7 '\n'
  , loop s
  ]
  where
  loop (x:xs) = mconcat
    [ buildFixed 10 '0' (elemOffset x)
    , char7 ' '
    , buildFixed 5 '0' (elemGen x)
    , char7 ' '
    , char7 (if elemFree x then 'f' else 'n')
    , string7 "\r\n"
    ] `mappend` loop xs
  loop [] = mempty

buildFixed :: Show a => Int -> Char -> a -> Builder
buildFixed len c i =
  let v = take len $ show i
      l = length v
  in string7 $ replicate (len - l) c ++ v
