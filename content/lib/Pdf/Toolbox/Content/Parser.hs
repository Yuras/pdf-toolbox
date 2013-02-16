
-- | Parse content stream

module Pdf.Toolbox.Content.Parser
(
  parseContentStream
)
where

import Data.Attoparsec.Char8 (Parser)
import qualified Data.Attoparsec.Char8 as Parser
import Control.Monad
import Control.Applicative
import System.IO.Streams (InputStream)
import qualified System.IO.Streams.Attoparsec as Streams

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Parsers.Object

import Pdf.Toolbox.Content.Ops

parseContent :: Parser (Maybe Expr)
parseContent
  = (Parser.skipSpace >> Parser.endOfInput >> return Nothing)
  <|> do
    Parser.skipSpace
    fmap Just $ fmap Obj parseObject <|> fmap (Op . toOp) (Parser.takeWhile1 isRegularChar)

-- | Convert steam of bytes to stream of expressions
parseContentStream :: MonadIO m => Stream IS -> m (Stream (InputStream Expr))
parseContentStream (Stream dict is) = Stream dict `liftM` liftIO (Streams.parserToInputStream parseContent is)
