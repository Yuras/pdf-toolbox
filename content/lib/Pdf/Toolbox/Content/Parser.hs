
-- | Parse content stream

module Pdf.Toolbox.Content.Parser
(
  parseContentStream,
  readNextOperator
)
where

import Data.Int
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import Data.IORef
import Control.Applicative
import Control.Exception
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Parsers.Object

import Pdf.Toolbox.Content.Ops

-- | Parse content streams for a page
--
-- Note: we need content stream ref to be able to decrypt stream content.
-- We need stream length because it can be an indirect object in
-- stream dictionary
parseContentStream
  :: Buffer
  -- ^ random input stream to read data from
  -> [StreamFilter]
  -- ^ how to unpack data
  -> (Ref -> InputStream ByteString -> IO (InputStream ByteString))
  -- ^ how to decrypt data
  -> [(Stream Int64, Ref, Int)]
  -- ^ content streams (with offset), their refs and length
  -> IO (InputStream Expr)
parseContentStream ris filters decryptor streams = do
  is <- combineStreams ris filters decryptor streams
  Streams.parserToInputStream parseContent is

-- | Read the next operator if any
readNextOperator :: InputStream Expr -> IO (Maybe Operator)
readNextOperator is = message "readNextOperator" $ go []
  where
  go args = do
    expr <- Streams.read is
      `catch` \(Streams.ParseException msg) -> throw (Corrupted msg [])
    case expr of
      Nothing -> case args of
                   [] -> return Nothing
                   _ -> throw $ Corrupted ("Args without op: " ++ show args) []
      Just (Obj o) -> go (o : args)
      Just (Op o) -> return $ Just (o, reverse args)

combineStreams
  :: Buffer
  -> [StreamFilter]
  -> (Ref -> InputStream ByteString -> IO (InputStream ByteString))
  -> [(Stream Int64, Ref, Int)]
  -> IO (InputStream ByteString)
combineStreams _ _ _ [] = Streams.nullInput
combineStreams buf filters decryptor (x:xs)
  = mkReader x xs >>= newIORef
  >>= Streams.makeInputStream . doRead
  where
  mkReader (s, ref, len) ss = do
    is <- decodedStreamContent buf filters (decryptor ref) len s
    return (is, ss)
  doRead ref = do
    (is, ss) <- readIORef ref
    chunk <- Streams.read is
    case chunk of
      Nothing ->
        case ss of
          [] -> return Nothing
          (h:t) -> do
            reader <- mkReader h t
            writeIORef ref reader
            doRead ref
      Just c -> return (Just c)

parseContent :: Parser (Maybe Expr)
parseContent
  = (skipSpace >> Parser.endOfInput >> return Nothing)
  <|> do
    skipSpace
    fmap Just $ fmap Obj parseObject <|>
                fmap (Op . toOp) (Parser.takeWhile1 isRegularChar)

-- Treat comments as spaces
skipSpace :: Parser ()
skipSpace = do
  Parser.skipSpace
  _ <- many $ do
    _ <- Parser.char '%'
    Parser.skipWhile $ \c -> c /= '\n' && c /= '\r'
    Parser.skipSpace
  return ()
