
-- | Unclassified tools

module Pdf.Toolbox.Core.Util
(
  notice,
  readObjectAtOffset,
  readCompressedObject
)
where

import Pdf.Toolbox.Core.IO.Buffer (Buffer)
import qualified Pdf.Toolbox.Core.IO.Buffer as Buffer
import Pdf.Toolbox.Core.Exception
import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Parsers.Object

import Data.Int
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import Control.Monad
import Control.Exception
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

-- | Add a message to 'Maybe'
notice :: Maybe a -> String -> Either String a
notice Nothing = Left
notice (Just a) = const (Right a)

-- | Read indirect object at the specified offset
--
-- Returns the object and the 'Ref'. The payload for stream
-- will be an offset of stream content
readObjectAtOffset :: Buffer
                   -> Int64   -- ^ object offset
                   -> IO (Ref, Object Int64)
readObjectAtOffset buf off = message "readObjectAtOffset" $ do
  Buffer.seek buf off
  (ref, o) <- Streams.parseFromStream parseIndirectObject
    (Buffer.toInputStream buf)
      `catch` \(Streams.ParseException msg) -> throw (Corrupted msg [])
  o' <-
    case o of
      Number val -> return $ Number val
      Bool val -> return $ Bool val
      Name val -> return $ Name val
      Dict val -> return $ Dict val
      Array val -> return $ Array val
      String val -> return $ String val
      Stream (S dict _) -> (Stream . S dict) <$> Buffer.tell buf
      Ref _ -> throw $ Corrupted "Indirect object can't be a Ref" []
      Null -> return Null
  return (ref, o')

-- | Read object from object stream
--
-- Never returns 'Stream'
readCompressedObject :: InputStream ByteString
                     -- ^ decoded object stream
                     -> Int64
                     -- ^ an offset of the first object
                     -- (\"First\" key in dictionary)
                     -> Int
                     -- ^ object number to read
                     -> IO (Object ())
readCompressedObject is first num = do
  (is', counter) <- Streams.countInput is
  off <- do
    res <- Streams.parseFromStream (replicateM (num + 1) headerP) is'
      `catch` \(Streams.ParseException msg) -> throwIO $ Corrupted
        "Object stream" [msg]
    when (null res) $
      error "readCompressedObject: imposible"
    case last res of
      (_, off) -> return off
  pos <- counter
  Buffer.dropExactly (fromIntegral $ first + off - pos) is
  Streams.parseFromStream parseObject is
    `catch` \(Streams.ParseException msg) -> throwIO $ Corrupted
      "Object in object stream" [msg]
  where
  headerP :: Parser (Int, Int64)
  headerP = do
    n <- Parser.decimal
    Parser.skipSpace
    off <- Parser.decimal
    Parser.skipSpace
    return (n, off)
