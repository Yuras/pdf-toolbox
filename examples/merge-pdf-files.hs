{-# LANGUAGE OverloadedStrings #-}

-- An example how to merge PDF files into one big file.
--
-- Usage:
--      ./merge-pdf-files in1.pdf in2.pdf ... > out.pdf
--
-- TODO: Encrypted files are not supported
-- TODO: Annotations, media box
-- TODO: Inherited resources

module Main
(
  main,
  liftApp
)
where

import Data.Int
import qualified Data.ByteString.Lazy as BSL
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import System.IO
import qualified System.IO.Streams as Streams
import System.Environment

import Pdf.Toolbox.Core
import Pdf.Toolbox.Document
import Pdf.Toolbox.Document.Internal.Types

data AppState = AppState {
  stNextFree :: Int,
  stPageRefs :: [Ref]
  }

initialAppState :: AppState
initialAppState = AppState {
  stNextFree = 1,
  stPageRefs = []
  }

nextFreeIndex :: Monad m => StateT AppState m Int
nextFreeIndex = do
  st <- get
  let index = stNextFree st
  put $ st {stNextFree = index + 1}
  return index

putPageRef :: Monad m => Ref -> StateT AppState m ()
putPageRef ref =
  modify $ \st -> st {stPageRefs = ref : stPageRefs st}

liftApp :: Monad m => StateT AppState m a -> Pdf (StateT AppState m) a
liftApp = lift . lift

liftWriter :: Monad m => PdfWriter m a -> Pdf (StateT AppState (PdfWriter m)) a
liftWriter = liftApp . lift

writeObjectChildren :: Object () -> Pdf (StateT AppState (PdfWriter IO)) (Object ())
writeObjectChildren (ORef r) = do
  o <- lookupObject r
  case o of
    OStream s -> do
      ref <- writeStream s
      return $ ORef ref
    _ -> do
      let o' = mapObject (error "impossible") o
      o'' <- writeObjectChildren o'
      index <- liftApp nextFreeIndex
      let ref = Ref index 0
      liftWriter $ writeObject ref $ mapObject (error "impossible") o''
      return $ ORef ref
writeObjectChildren (ODict (Dict vals)) = do
  vals' <- forM vals $ \(key, val) -> do
    val' <- writeObjectChildren val
    return (key, val')
  return $ ODict $ Dict vals'
writeObjectChildren (OArray (Array vals)) = do
  vals' <- forM vals writeObjectChildren
  return $ OArray $ Array vals'
writeObjectChildren o = return o

writeStream :: Stream Int64 -> Pdf (StateT AppState (PdfWriter IO)) Ref
writeStream s@(Stream dict _) = do
    len <- lookupDict "Length" dict >>= deref >>= fromObject >>= intValue
    ris <- getRIS
    Stream _ is <- rawStreamContent ris len s
    content <- liftIO $ BSL.fromChunks `liftM` Streams.toList is
    index <- liftApp nextFreeIndex
    let ref = Ref index 0
    dict' <- writeObjectChildren (ODict dict) >>= fromObject
    liftWriter $ writeObject ref $ OStream $ Stream dict' content
    return ref

writePdfPage :: Page -> Pdf (StateT AppState (PdfWriter IO)) ()
writePdfPage page@(Page _ pageDict) = do
  pageIndex <- liftApp nextFreeIndex
  let pageRef = Ref pageIndex 0
  liftApp $ putPageRef pageRef
  contentRefs <- pageContents page
  contentRefs' <- forM contentRefs $ \r -> do
    s <- lookupObject r >>= toStream
    writeStream s
  resources <- lookupDict "Resources" pageDict >>= deref >>= writeObjectChildren
  liftWriter $ writeObject pageRef $ ODict $ Dict [
    ("Type", OName "Page"),
    ("Contents", OArray $ Array $ map ORef contentRefs'),
    ("Resources", resources)
    ]

writePdfFile :: FilePath -> StateT AppState (PdfWriter IO) ()
writePdfFile path = do
  handle <- liftIO $ openBinaryFile path ReadMode
  res <- runPdfWithHandle handle knownFilters $ do
    encrypted <- isEncrypted
    when encrypted $ setUserPassword defaultUserPassord
    root <- document >>= documentCatalog >>= catalogPageNode
    count <- pageNodeNKids root
    forM_ [0..count-1] $ \i -> do
      page <- pageNodePageByNum root i
      writePdfPage page
  when (isLeft res) $ error $ show res
  liftIO $ hClose handle

writeTrailer :: StateT AppState (PdfWriter IO) ()
writeTrailer = do
  pageRefs <- gets stPageRefs

  rootIndex <- nextFreeIndex
  let rootRef = Ref rootIndex 0
  lift $ writeObject rootRef $ ODict $ Dict [
    ("Type", OName "Pages"),
    ("Count", ONumber $ NumInt $ length pageRefs),
    ("Kids", OArray $ Array $ map ORef $ reverse pageRefs)
    ]

  catalogIndex <- nextFreeIndex
  let catalogRef = Ref catalogIndex 0
  lift $ writeObject catalogRef $ ODict $ Dict [("Type", OName "Catalog"), ("Pages", ORef rootRef)]

  count <- gets stNextFree
  lift $ writeXRefTable 0 (Dict [("Size", ONumber $ NumInt $ count - 1), ("Root", ORef catalogRef)])

main :: IO ()
main = do
  files <- getArgs
  runPdfWriter Streams.stdout $ do
    writePdfHeader
    flip evalStateT initialAppState $ do
      forM_ files writePdfFile
      writeTrailer
  return ()
