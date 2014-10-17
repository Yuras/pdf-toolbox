
-- | Buffer abstracts from file IO

module Pdf.Toolbox.Core.IO.Buffer
(
  Buffer(..),
  bufferToInputStream,
  handleToBuffer,
  bytesToBuffer,
  dropExactly
)
where

import Data.Int
import Data.IORef
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Control.Applicative
import Control.Monad
import System.IO
import qualified System.IO.Streams as Streams
import System.IO.Streams.Internal (InputStream(..))
import qualified System.IO.Streams.Internal as Streams

-- | Interface to file
data Buffer = Buffer
  { bufferRead :: IO (Maybe ByteString)
  , bufferSize :: IO Int64
  , bufferSeek :: Int64 -> IO ()
  , bufferBack :: Int64 -> IO ()
  , bufferTell :: IO Int64
  }

-- | Convert buffer to 'InputStream'
bufferToInputStream :: Buffer -> InputStream ByteString
bufferToInputStream buf = InputStream
  { Streams._read = bufferRead buf
  , Streams._unRead = bufferBack buf . fromIntegral . ByteString.length
  }

-- | Make buffer from handle
--
-- Don't touch the handle while using buffer
handleToBuffer :: Handle -> IO Buffer
-- it is in IO in case we'll need to store intermediate state
handleToBuffer h = return $ Buffer
  { bufferRead = do
      bs <- ByteString.hGetSome h defaultSize
      if ByteString.null bs
        then return Nothing
        else return (Just bs)
  , bufferSize = fromIntegral <$> hFileSize h
  , bufferSeek = hSeek h AbsoluteSeek . fromIntegral
  , bufferBack = hSeek h RelativeSeek . negate . fromIntegral
  , bufferTell = fromIntegral <$> hTell h
  }

-- | Buffer from strict 'ByteString'
--
-- That is mostly for testing
bytesToBuffer :: ByteString -> IO Buffer
bytesToBuffer bs = do
  ref <- newIORef 0
  return Buffer
    { bufferRead = do
        pos <- readIORef ref
        let chunk = ByteString.drop pos bs
        modifyIORef ref (+ ByteString.length chunk)
        if ByteString.null chunk
          then return Nothing
          else return (Just chunk)
    , bufferSeek = writeIORef ref . fromIntegral
    , bufferSize = return $ fromIntegral (ByteString.length bs)
    , bufferBack = modifyIORef ref . flip (-) . fromIntegral
    , bufferTell = fromIntegral <$> readIORef ref
    }

-- | Drop specified number of bytes from imput stream
dropExactly :: Int -> InputStream ByteString -> IO ()
-- XXX: it seams to produce the intermediate bytestring
dropExactly n = void . Streams.readExactly n

defaultSize :: Int
defaultSize = 32752
