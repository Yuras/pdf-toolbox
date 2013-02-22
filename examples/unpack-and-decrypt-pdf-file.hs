{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Data.Int
import qualified Data.ByteString.Lazy as BSL
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Environment
import System.IO
import qualified System.IO.Streams as Streams

import Pdf.Toolbox.Core
import Pdf.Toolbox.Document
import Pdf.Toolbox.Document.Internal.Types

main :: IO ()
main = do
  [input] <- getArgs
  res <- withBinaryFile input ReadMode $ \handle ->
    runPdfWithHandle handle knownFilters $ do
    encrypted <- isEncrypted
    when encrypted $ setUserPassword defaultUserPassord
    Document _ tr <- document
    Catalog ref dict <- document >>= documentCatalog

    let
        loop :: Object Int64 -> StateT (Set Ref) (PdfWriter (Pdf IO)) ()
        loop (ODict (Dict vals)) = forM_ vals $ loop . mapObject (error "impossible") . snd
        loop (OArray (Array vals)) = forM_ vals $ loop . mapObject (error "impossible")
        loop (ORef r) = do
          member <- gets $ Set.member r
          if member
            then return ()
            else do
              o <- lift $ lift $ lookupObject r
              lift (loadStream r o) >>= lift . writeObject r
              modify $ Set.insert r
              loop o
        loop _ = return ()

        loadStream r (OStream s) = do
          Stream d is <- lift $ streamContent r s
          content <- liftIO $ BSL.fromChunks `liftM` Streams.toList is
          let d' = setValueForKey "Length" (ONumber $ NumInt $ fromIntegral $ BSL.length content) $ deleteValueForKey "Filter" d
          return $ OStream $ Stream d' content
        loadStream _ o = return $ mapObject (error "impossible") o

    runPdfWriter Streams.stdout $ do
      flip evalStateT Set.empty $ do
        lift writePdfHeader
        lift $ writeObject ref (ODict dict)
        loop (ODict tr)
        lift $ writeXRefTable 0 (deleteValueForKey "Prev" tr)
  print res
  return ()
