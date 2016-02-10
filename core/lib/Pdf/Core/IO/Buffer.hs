
-- | Buffer abstracts from file IO

module Pdf.Core.IO.Buffer
(
  Buffer(..),
  toInputStream,
  fromHandle,
  fromBytes,
  dropExactly
)
where

import Prelude hiding (read)
import Data.Int
import Data.IORef
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Control.Monad
import System.IO
import qualified System.IO.Streams as Streams
import System.IO.Streams.Internal (InputStream(..))
import qualified System.IO.Streams.Internal as Streams

-- | Interface to file
data Buffer = Buffer
  { read :: IO (Maybe ByteString)
  , size :: IO Int64
  , seek :: Int64 -> IO ()
  , back :: Int64 -> IO ()
  , tell :: IO Int64
  }

-- | Convert buffer to 'InputStream'
toInputStream :: Buffer -> InputStream ByteString
toInputStream buf = InputStream
  { Streams._read = read buf
  , Streams._unRead = back buf . fromIntegral . ByteString.length
  }

-- | Make buffer from handle
--
-- Don't touch the handle while using buffer
fromHandle :: Handle -> IO Buffer
-- it is in IO in case we'll need to store intermediate state
fromHandle h = return $ Buffer
  { read = do
      bs <- ByteString.hGetSome h defaultSize
      if ByteString.null bs
        then return Nothing
        else return (Just bs)
  , size = fromIntegral <$> hFileSize h
  , seek = hSeek h AbsoluteSeek . fromIntegral
  , back = hSeek h RelativeSeek . negate . fromIntegral
  , tell = fromIntegral <$> hTell h
  }

-- | Buffer from strict 'ByteString'
--
-- That is mostly for testing
fromBytes :: ByteString -> IO Buffer
fromBytes bs = do
  ref <- newIORef 0
  return Buffer
    { read = do
        pos <- readIORef ref
        let chunk = ByteString.drop pos bs
        modifyIORef ref (+ ByteString.length chunk)
        if ByteString.null chunk
          then return Nothing
          else return (Just chunk)
    , seek = writeIORef ref . fromIntegral
    , size = return $ fromIntegral (ByteString.length bs)
    , back = modifyIORef ref . flip (-) . fromIntegral
    , tell = fromIntegral <$> readIORef ref
    }

-- | Drop specified number of bytes from input stream
dropExactly :: Int -> InputStream ByteString -> IO ()
dropExactly n = void . Streams.readExactly n

defaultSize :: Int
defaultSize = 32752
