{-# LANGUAGE OverloadedStrings #-}

-- | Document info dictionary

module Pdf.Document.Info
(
  infoTitle,
  infoAuthor,
  infoSubject,
  infoKeywords,
  infoCreator,
  infoProducer,
)
where

import Pdf.Core.Exception
import Pdf.Core.Util
import Pdf.Core.Object.Util

import Pdf.Document.Pdf
import Pdf.Document.Internal.Types
import Pdf.Document.Internal.Util

import Data.Text (Text)
import qualified Data.HashMap.Strict as HashMap

-- | Document title
infoTitle :: Info -> IO (Maybe Text)
infoTitle (Info pdf _ dict) =
  case HashMap.lookup "Title" dict of
    Nothing -> return Nothing
    Just o -> do
      o' <- deref pdf o
      mstr <- sure $ fmap Just (stringValue o') `notice` "Title should be a string"
      case mstr of
        Nothing -> return Nothing
        Just str -> Just <$> decodeTextStringThrow str

-- | The name of the person who created the document
infoAuthor :: Info -> IO (Maybe Text)
infoAuthor (Info pdf _ dict) =
  case HashMap.lookup "Author" dict of
    Nothing -> return Nothing
    Just o -> do
      o' <- deref pdf o
      mstr <- sure $ fmap Just (stringValue o') `notice` "Author should be a string"
      case mstr of
        Nothing -> return Nothing
        Just str -> Just <$> decodeTextStringThrow str

-- | The subject of the document
infoSubject :: Info -> IO (Maybe Text)
infoSubject (Info pdf _ dict) =
  case HashMap.lookup "Subject" dict of
    Nothing -> return Nothing
    Just o -> do
      o' <- deref pdf o
      mstr <- sure $ fmap Just (stringValue o') `notice` "Subject should be a string"
      case mstr of
        Nothing -> return Nothing
        Just str -> Just <$> decodeTextStringThrow str

-- | Keywords associated with the document
infoKeywords :: Info -> IO (Maybe Text)
infoKeywords (Info pdf _ dict) =
  case HashMap.lookup "Keywords" dict of
    Nothing -> return Nothing
    Just o -> do
      o' <- deref pdf o
      mstr <- sure $ fmap Just (stringValue o') `notice` "Keywords should be a string"
      case mstr of
        Nothing -> return Nothing
        Just str -> Just <$> decodeTextStringThrow str

-- | The name of the application that created the original document
infoCreator :: Info -> IO (Maybe Text)
infoCreator (Info pdf _ dict) =
  case HashMap.lookup "Creator" dict of
    Nothing -> return Nothing
    Just o -> do
      o' <- deref pdf o
      mstr <- sure $ fmap Just (stringValue o') `notice` "Creator should be a string"
      case mstr of
        Nothing -> return Nothing
        Just str -> Just <$> decodeTextStringThrow str

-- | The name of the application that converted the document to PDF format
infoProducer :: Info -> IO (Maybe Text)
infoProducer (Info pdf _ dict) =
  case HashMap.lookup "Producer" dict of
    Nothing -> return Nothing
    Just o -> do
      o' <- deref pdf o
      mstr <- sure $ fmap Just (stringValue o') `notice` "Producer should be a string"
      case mstr of
        Nothing -> return Nothing
        Just str -> Just <$> decodeTextStringThrow str
