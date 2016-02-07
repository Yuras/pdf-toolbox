{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Data.IORef
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString
import qualified Data.Foldable as Foldable
import qualified Data.Vector as Vector
import qualified Data.IntSet as IntSet
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Exception
import System.Environment
import System.IO
import qualified System.IO.Streams as Streams

import Pdf.Toolbox.Core hiding (rawStreamContent)
import Pdf.Toolbox.Document
import Pdf.Toolbox.Document.Encryption
import Pdf.Toolbox.Document.Internal.Types

main :: IO ()
main = do
  [input] <- getArgs
  withBinaryFile input ReadMode $ \h -> do
    pdf <- pdfWithHandle h

    encrypted <- isEncrypted pdf
    when encrypted $ do
      ok <- setUserPassword pdf defaultUserPassword
      unless ok $
        error "Wrong password"

    Document _ tr <- document pdf

    writer <- makeWriter Streams.stdout

    stateRef <- newIORef IntSet.empty
    let
        loop (Dict vals) = Foldable.forM_ vals loop
        loop (Array vals) = Vector.forM_ vals loop
        loop (Ref r@(R index _)) = do
          -- check that the object is not already written.
          -- necessary to prevent circles
          member <- IntSet.member index <$> readIORef stateRef
          if member
            then return ()
            else do
              o <- lookupObject pdf r
              case o of
                Stream s -> do
                  (d, dat) <- loadStream pdf r s
                  writeStream writer r d dat
                _ -> writeObject writer r o
              modifyIORef stateRef $ IntSet.insert index
              loop o
        loop (Stream (S d _)) = loop (Dict d)
        loop _ = return ()

    writeHeader writer
    -- traverse all the objects starting from trailer
    -- and write out all the indirect objects found
    loop (Dict tr)
    -- There are no more xrefs, so clean prev key
    writeXRefTable writer 0 (HashMap.delete "Prev" tr)

loadStream :: Pdf -> Ref -> Stream -> IO (Dict, Lazy.ByteString)
loadStream pdf ref s = do
  res <- loadDecodedStream pdf ref s
    `catch` \Corrupted{} -> loadRawStream pdf ref s
  return res

loadDecodedStream :: Pdf -> Ref -> Stream -> IO (Dict, Lazy.ByteString)
loadDecodedStream pdf ref s@(S d _) = do
  is <- streamContent pdf ref s
  cont <- Lazy.ByteString.fromChunks <$> Streams.toList is
  -- update length and remove filter
  let d' = HashMap.insert "Length" (Number len)
         . HashMap.delete "Filter"
         $ d
      len = fromIntegral (Lazy.ByteString.length cont)
  return (d', cont)

loadRawStream :: Pdf -> Ref -> Stream -> IO (Dict, Lazy.ByteString)
loadRawStream pdf _ s@(S d _) = do
  is <- rawStreamContent pdf s
  cont <- Lazy.ByteString.fromChunks <$> Streams.toList is
  return (d, cont)
