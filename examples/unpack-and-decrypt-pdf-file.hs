{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Data.Int
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString
import qualified Data.Foldable as Foldable
import qualified Data.Vector as Vector
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
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

    let
        loop :: Object Int64 -> StateT IntSet (PdfWriter IO) ()
        loop (Dict vals) = Foldable.forM_ vals $ \v ->
          loop (mapObject (error "impossible") v)
        loop (Array vals) = Vector.forM_ vals $ \v ->
          loop (mapObject (error "impossible") v)
        loop (Ref r@(R index _)) = do
          -- check that the object is not already written.
          -- necessary to prevent circles
          member <- gets $ IntSet.member index
          if member
            then return ()
            else do
              o <- lift $ lift $ lookupObject pdf r
              lift (lift (loadStream pdf r o) >>= writeObject r)
              modify $ IntSet.insert index
              loop o
        loop (Stream (S d _)) = loop (Dict d)
        loop _ = return ()

    runPdfWriter Streams.stdout $ do
      flip evalStateT IntSet.empty $ do
        lift writePdfHeader
        -- traverse all the objects starting from trailer
        -- and write out all the indirect objects found
        loop (Dict tr)
        -- There are no more xrefs, so clean prev key
        lift $ writeXRefTable 0 (HashMap.delete "Prev" tr)

    return ()

mapObject :: (a -> b) -> Object a -> Object b
mapObject _ (Dict d) = Dict d
mapObject _ (Name n) = Name n
mapObject _ (String s) = String s
mapObject _ (Number n) = Number n
mapObject _ (Bool b) = Bool b
mapObject _ (Array a) = Array a
mapObject _ (Ref r) = Ref r
mapObject _ Null = Null
mapObject f (Stream (S d a)) = Stream (S d (f a))

loadStream :: Pdf -> Ref -> Object Int64 -> IO (Object Lazy.ByteString)
loadStream pdf ref (Stream s) = do
  s' <- loadDecodedStream pdf ref s
    `catch` \Corrupted{} -> loadRawStream pdf ref s
  return (Stream s')
loadStream _ _ o = return (mapObject (error "impossible") o)

loadDecodedStream :: Pdf -> Ref -> Stream Int64 -> IO (Stream Lazy.ByteString)
loadDecodedStream pdf ref s = do
  S d is <- streamContent pdf ref s
  cont <- Lazy.ByteString.fromChunks <$> Streams.toList is
  -- update length and remove filter
  let d' = HashMap.insert "Length" (Number len)
         . HashMap.delete "Filter"
         $ d
      len = fromIntegral (Lazy.ByteString.length cont)
  return (S d' cont)

loadRawStream :: Pdf -> Ref -> Stream Int64 -> IO (Stream Lazy.ByteString)
loadRawStream pdf _ s = do
  S d is <- rawStreamContent pdf s
  cont <- Lazy.ByteString.fromChunks <$> Streams.toList is
  return (S d cont)
