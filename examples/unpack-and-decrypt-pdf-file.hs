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
    runPdfWithHandle handle knownFilters $
    -- we take each object only once, and object cache
    -- will only slow down everything
    withoutObjectCache $ do
    encrypted <- isEncrypted
    when encrypted $ setUserPassword defaultUserPassord
    Document _ tr <- document

    let
        loop :: Object Int64 -> StateT (Set Ref) (PdfWriter (Pdf IO)) ()
        loop (ODict (Dict vals)) = forM_ vals $ loop . mapObject (error "impossible") . snd
        loop (OArray (Array vals)) = forM_ vals $ loop . mapObject (error "impossible")
        loop (ORef r) = do
          -- check that the object is not already written.
          -- necessary to prevent circles
          member <- gets $ Set.member r
          if member
            then return ()
            else do
              o <- lift $ lift $ lookupObject r
              lift ( lift $ loadStream r o) >>= lift . writeObject r
              modify $ Set.insert r
              loop o
        loop _ = return ()

    runPdfWriter Streams.stdout $ do
      flip evalStateT Set.empty $ do
        lift writePdfHeader
        -- traverse all the objects starting from trailer
        -- and write out all the indirect objects found
        loop (ODict tr)
        -- There are no more xrefs, so clean prev key
        lift $ writeXRefTable 0 (deleteValueForKey "Prev" tr)
  case res of
    Right _ -> return ()
    Left e -> hPutStrLn stderr (show e)

-- Load stream content as lazy bytestring. Try to decode it,
-- but if it is not possible (e.g. streams filter is not found),
-- then load raw content. It may not work with encrypted documents
loadStream :: Ref -> Object Int64 -> Pdf IO (Object BSL.ByteString)
loadStream r (OStream s) = loadDecodedStream r s `catchT` \_ -> loadRawStream s
loadStream _ o = return $ mapObject (error "impossible") o

loadDecodedStream :: Ref -> Stream Int64 -> Pdf IO (Object BSL.ByteString)
loadDecodedStream r s = do
  Stream d is <- streamContent r s
  content <- liftIO $ BSL.fromChunks `liftM` Streams.toList is
  -- update length and remove filter
  let d' = setValueForKey "Length" (ONumber $ NumInt $ fromIntegral $ BSL.length content) $ deleteValueForKey "Filter" d
  return $ OStream $ Stream d' content

loadRawStream :: Stream Int64 -> Pdf IO (Object BSL.ByteString)
loadRawStream s@(Stream d _) = do
  l <- lookupDict "Length" d >>= deref >>= fromObject >>= intValue
  ris <- getRIS
  Stream _ is <- rawStreamContent ris l s
  content <- liftIO $ BSL.fromChunks `liftM` Streams.toList is
  return $ OStream $ Stream d content
