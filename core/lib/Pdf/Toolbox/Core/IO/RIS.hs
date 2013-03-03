
-- | Input stream with random access

module Pdf.Toolbox.Core.IO.RIS
(
  IS,
  RIS(..),
  RIS'(..),
  seek,
  size,
  tell,
  inputStream,
  fromHandle,
  fromHandle'
)
where

import Data.Int (Int64)
import Data.Functor
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import System.IO
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams

-- | Sequential input stream
type IS = InputStream ByteString

-- | Internal state of 'RIS'
data RIS' = RIS' {
  risSeek :: Int64 -> IO (IO (Maybe ByteString)),
  risInputStream :: IS,
  risPos :: IO Int64,
  risSize :: Int64
  }

-- | Random access Input Stream
newtype RIS = RIS (IORef RIS')

-- | Seek the stream
seek :: RIS -> Int64 -> IO ()
seek (RIS ref) pos = do
  ris <- readIORef ref
  source <- risSeek ris pos
  stream <- Streams.makeInputStream source
  (s, c) <- Streams.countInput stream
  writeIORef ref ris {
    risInputStream = s,
    risPos = (+ pos) <$> c
    }

-- | Create RIS from 'Handle' with the specified chunk size
fromHandle' :: Handle -> Int -> IO RIS
fromHandle' h buf = do
  sz <- hFileSize h
  posRef <- newIORef 0
  stream <- Streams.makeInputStream (f posRef)
  (s, c) <- Streams.countInput stream
  RIS <$> newIORef RIS' {
    risSeek = \pos -> do
      hSeek h AbsoluteSeek (fromIntegral pos)
      ref <- newIORef $ fromIntegral pos
      return $ f ref,
    risInputStream = s,
    risPos = c,
    risSize = fromIntegral sz
    }
  where
  f ref = do
    prevPos <- readIORef ref
    curtPos <- hTell h
    hSeek h AbsoluteSeek prevPos
    chunk <- BS.hGetSome h buf
    hSeek h AbsoluteSeek curtPos
    writeIORef ref $! prevPos + fromIntegral (BS.length chunk)
    return $! if BS.null chunk then Nothing else Just chunk

-- | Create RIS from 'Handle' with default chunk size
fromHandle :: Handle -> IO RIS
fromHandle h = fromHandle' h 32752

-- | Number of bytes in the stream
size :: RIS -> IO Int64
size (RIS ref) = risSize <$> readIORef ref

-- | Current position in bytes
tell :: RIS -> IO Int64
tell (RIS ref) = readIORef ref >>= risPos

-- | Get sequential input stream, that is valid until the next 'seek'
inputStream :: RIS -> IO IS
inputStream (RIS ref) = risInputStream <$> readIORef ref
