{-# LANGUAGE OverloadedStrings #-}

-- An example how to merge PDF files into one big file.
--
-- Usage:
--      ./merge-pdf-files in1.pdf in2.pdf ... > out.pdf
--
-- TODO: Encrypted files are not supported
-- TODO: Annotations, media box
-- TODO: Inherited resources
-- TODO: Resources (fonts, etc) are written a number of times, check for dublicates

module Main
(
  main,
)
where

import Pdf.Core.Object
import Pdf.Core.Writer
import Pdf.Document
import Pdf.Document.Internal.Types

import Data.IORef
import qualified Data.ByteString.Lazy as Lazy.ByteString
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import qualified System.IO.Streams as Streams
import System.Environment

data AppState = AppState {
  stNextFree :: Int,
  stPageRefs :: [Ref],
  stRootNode :: Ref
  }

initialAppState :: AppState
initialAppState = AppState {
  stNextFree = 1,
  stPageRefs = [],
  stRootNode = error "stRootNode"
  }

nextFreeIndex :: IORef AppState -> IO Int
nextFreeIndex stateRef = do
  st <- readIORef stateRef
  let index = stNextFree st
  writeIORef stateRef $ st {stNextFree = index + 1}
  return index

putPageRef :: IORef AppState -> Ref -> IO ()
putPageRef stateRef ref =
  modifyIORef stateRef $ \st -> st {stPageRefs = ref : stPageRefs st}

main :: IO ()
main = do
  files <- getArgs
  writer <- makeWriter Streams.stdout
  writeHeader writer
  deleteObject writer (R 0 65535) 0
  stateRef <- newIORef initialAppState
  index <- nextFreeIndex stateRef
  modifyIORef stateRef $ \st -> st {stRootNode = R index 0}
  forM_ files $
    writePdfFile writer stateRef
  writeTrailer writer stateRef

writePdfFile :: Writer -> IORef AppState -> FilePath -> IO ()
writePdfFile writer stateRef path = withPdfFile path $ \pdf -> do
  encrypted <- isEncrypted pdf
  when encrypted $ do
    ok <- setUserPassword pdf defaultUserPassword
    unless ok $
      error "Wrong password"

  root <- document pdf >>= documentCatalog >>= catalogPageNode
  count <- pageNodeNKids root
  forM_ [0..count-1] $ \i -> do
    page <- pageNodePageByNum root i
    writePdfPage writer stateRef page

  {-
  handle <- openBinaryFile path ReadMode

  pdf <- do
    pdf <- pdfWithHandle handle

    encrypted <- isEncrypted pdf
    when encrypted $ do
      ok <- setUserPassword pdf defaultUserPassword
      unless ok $
        error "Wrong password"
    return pdf

  root <- document pdf >>= documentCatalog >>= catalogPageNode
  count <- pageNodeNKids root
  forM_ [0..count-1] $ \i -> do
    page <- pageNodePageByNum root i
    writePdfPage writer stateRef page

  hClose handle
  -}

writePdfPage :: Writer -> IORef AppState -> Page -> IO ()
writePdfPage writer stateRef page@(Page pdf _ pageDict) = do
  parentRef <- stRootNode <$> readIORef stateRef
  pageIndex <- nextFreeIndex stateRef
  let pageRef = R pageIndex 0
  putPageRef stateRef pageRef
  contentRefs <- pageContents page
  contentRefs' <- forM contentRefs $ \r -> do
    o <- lookupObject pdf r
    case o of
      Stream s -> writeStream' writer stateRef pdf r s
      _ -> error "stream expected"

  resources <- do
    case HashMap.lookup "Resources" pageDict of
      Nothing -> error "No resources"
      Just v -> do
        o <- deref pdf v
        writeObjectChildren writer stateRef pdf o
  writeObject writer pageRef $ Dict $ HashMap.fromList [
    ("Type", Name "Page"),
    ("Contents", Array $ Vector.fromList $ map Ref contentRefs'),
    ("Resources", resources),
    ("Parent", Ref parentRef)
    ]

writeTrailer :: Writer -> IORef AppState -> IO ()
writeTrailer writer stateRef = do
  pageRefs <- stPageRefs <$> readIORef stateRef

  rootRef <- stRootNode <$> readIORef stateRef
  writeObject writer rootRef $ Dict $ HashMap.fromList [
    ("Type", Name "Pages"),
    ("Count", Number $ fromIntegral $ length pageRefs),
    ("Kids", Array $ Vector.fromList $ map Ref $ reverse pageRefs)
    ]

  catalogIndex <- nextFreeIndex stateRef
  let catalogRef = R catalogIndex 0
  writeObject writer catalogRef $ Dict $ HashMap.fromList
    [ ("Type", Name "Catalog")
    , ("Pages", Ref rootRef)
    ]

  count <- stNextFree <$> readIORef stateRef
  writeXRefTable writer 0 (HashMap.fromList
    [ ("Size", Number $ fromIntegral $ count - 1)
    , ("Root", Ref catalogRef)
    ])

writeStream' :: Writer -> IORef AppState -> Pdf -> Ref -> Stream -> IO Ref
writeStream' writer stateRef pdf ref s@(S dict _) = do
  cont <- do
    is <- rawStreamContent pdf ref s
    Lazy.ByteString.fromChunks <$> Streams.toList is

  Dict dict' <- writeObjectChildren writer stateRef pdf (Dict dict)

  index <- nextFreeIndex stateRef
  let r = R index 0
  writeStream writer r dict' cont
  return r

writeObjectChildren :: Writer -> IORef AppState -> Pdf -> Object -> IO Object
writeObjectChildren writer stateRef pdf (Ref r) = do
  o <- lookupObject pdf r
  case o of
    Stream s -> do
      ref <- writeStream' writer stateRef pdf r s
      return $ Ref ref
    _ -> do
      o' <- writeObjectChildren writer stateRef pdf o
      index <- nextFreeIndex stateRef
      let ref = R index 0
      writeObject writer ref o'
      return $ Ref ref
writeObjectChildren writer stateRef pdf (Dict vals) = do
  vals' <- forM (HashMap.toList vals) $ \(key, val) -> do
    val' <- writeObjectChildren writer stateRef pdf val
    return (key, val')
  return $ Dict $ HashMap.fromList vals'
writeObjectChildren writer stateRef pdf (Array vals) = do
  vals' <- Vector.forM vals (writeObjectChildren writer stateRef pdf)
  return $ Array vals'
writeObjectChildren _ _ _ o = return o
