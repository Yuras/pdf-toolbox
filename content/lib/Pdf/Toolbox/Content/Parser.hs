{-# LANGUAGE OverloadedStrings #-}

-- | Parse content stream

module Pdf.Toolbox.Content.Parser
(
  Op(..),
  Expr(..),
  parseContentStream
)
where

import Data.ByteString (ByteString)
import Data.Attoparsec.Char8 (Parser)
import qualified Data.Attoparsec.Char8 as Parser
import Control.Monad
import Control.Applicative
import System.IO.Streams (InputStream)
import qualified System.IO.Streams.Attoparsec as Streams

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Parsers.Object

-- | Content stream operators
data Op
  = Op_q
  | Op_Q
  | Op_cm
  | Op_w
  | Op_J
  | Op_j
  | Op_M
  | Op_d
  | UnknownOp ByteString
  deriving (Show, Eq)

-- | Expression is a regular objects or an operators
data Expr
  = Obj (Object ())
  | Op Op
  deriving (Show, Eq)

parseContent :: Parser (Maybe Expr)
parseContent
  = (Parser.endOfInput >> return Nothing)
  <|> do
    Parser.skipSpace
    fmap Just $ fmap Obj parseObject <|> fmap (Op . toOp) (Parser.takeWhile isRegularChar)

toOp :: ByteString -> Op
toOp "q" = Op_q
toOp str = UnknownOp str

-- | Convert steam of bytes to stream of expressions
parseContentStream :: MonadIO m => Stream IS -> m (Stream (InputStream Expr))
parseContentStream (Stream dict is) = Stream dict `liftM` liftIO (Streams.parserToInputStream parseContent is)
