
-- | Parse content stream

module Pdf.Toolbox.Content.Parser
(
  parseContentStream,
  readNextOperator
)
where

import Data.Int
import Data.Attoparsec.Char8 (Parser)
import qualified Data.Attoparsec.Char8 as Parser
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
parseContentStream :: MonadIO m
                   => RIS                         -- ^ random input stream to read data from
                   -> [StreamFilter]              -- ^ how to unpack data
                   -> (Ref -> IS -> IO IS)        -- ^ how to decrypt data
                   -> [(Stream Int64, Ref, Int)]  -- ^ content streams (with offset), their refs and length
                   -> PdfE m (InputStream Expr)
parseContentStream ris filters decryptor streams = do
  is <- combineStreams ris filters decryptor streams
  liftIO $ Streams.parserToInputStream parseContent is

-- | Read the next operator if any
readNextOperator :: MonadIO m => InputStream Expr -> PdfE m (Maybe Operator)
readNextOperator is = annotateError "reading the next operator from content stream" $ go []
  where
  go args = do
    expr <- do
      e <- tryPdfIO $ (Right <$> Streams.read is)
        `catch` (\e -> return $ Left $ UnexpectedError $ show (e :: Streams.ParseException))
      case e of
        Right expr -> return expr
        Left er -> left er
    case expr of
      Nothing -> case args of
                   [] -> return Nothing
                   _ -> left $ UnexpectedError $ "Args without op: " ++ show args
      Just (Obj o) -> go (o : args)
      Just (Op o) -> return $ Just (o, reverse args)

combineStreams :: MonadIO m => RIS -> [StreamFilter] -> (Ref -> IS -> IO IS) -> [(Stream Int64, Ref, Int)] -> PdfE m IS
combineStreams _ _ _ [] = liftIO Streams.nullInput
combineStreams ris filters decryptor (x:xs) = do
  reader <- mkReader x xs
  ref <- liftIO $ newIORef reader
  liftIO $ Streams.makeInputStream (doRead ref)
  where
  mkReader (s, ref, len) ss = do
    Stream _ is <- decodedStreamContent ris filters (decryptor ref) len s
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
  = (skipSpace >> Parser.endOfInput >> return Nothing)
  <|> do
    skipSpace
    fmap Just $ fmap Obj parseObject <|> fmap (Op . toOp) (Parser.takeWhile1 isRegularChar)

-- Treat comments as spaces
skipSpace :: Parser ()
skipSpace = do
  Parser.skipSpace
  _ <- many $ do
    _ <- Parser.char '%'
    Parser.skipWhile $ \c -> c /= '\n' && c /= '\r'
    Parser.skipSpace
  return ()
