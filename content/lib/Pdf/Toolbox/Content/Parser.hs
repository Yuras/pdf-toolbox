
-- | Parse content stream

module Pdf.Toolbox.Content.Parser
(
  parseContentStream
)
where

import Data.Int
import Data.Attoparsec.Char8 (Parser)
import qualified Data.Attoparsec.Char8 as Parser
import Data.IORef
import Control.Applicative
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Parsers.Object

import Pdf.Toolbox.Content.Ops

-- | Parse content streams for a page
parseContentStream :: MonadIO m => RIS -> [StreamFilter] -> [(Stream Int64, Int)] -> PdfE m (InputStream Expr)
parseContentStream ris filters streams = do
  is <- combineStreams ris filters streams
  liftIO $ Streams.parserToInputStream parseContent is

combineStreams :: MonadIO m => RIS -> [StreamFilter] -> [(Stream Int64, Int)] -> PdfE m IS
combineStreams _ _ [] = liftIO Streams.nullInput
combineStreams ris filters (x:xs) = do
  reader <- mkReader x xs
  ref <- liftIO $ newIORef reader
  liftIO $ Streams.makeInputStream (doRead ref)
  where
  mkReader (s, len) ss = do
    Stream _ is <- decodedStreamContent ris filters len s
    return (is, ss)
  doRead ref = do
    (is, ss) <- liftIO $ readIORef ref
    chunk <- liftIO $ Streams.read is
    case chunk of
      Nothing ->
        case ss of
          [] -> return Nothing
          (h:t) -> do
            reader <- runEitherT $ mkReader h t
            case reader of
              Left e -> liftIO $ ioError $ userError $ show e
              Right r -> do
                liftIO $ writeIORef ref r
                doRead ref
      Just c -> return (Just c)

parseContent :: Parser (Maybe Expr)
parseContent
  = (Parser.skipSpace >> Parser.endOfInput >> return Nothing)
  <|> do
    Parser.skipSpace
    fmap Just $ fmap Obj parseObject <|> fmap (Op . toOp) (Parser.takeWhile1 isRegularChar)
