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

import Data.Int
import Data.IORef
import qualified Data.ByteString.Lazy as Lazy.ByteString
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import qualified System.IO.Streams as Streams
import System.Environment

import Pdf.Toolbox.Core hiding (rawStreamContent)
import Pdf.Toolbox.Document
import Pdf.Toolbox.Document.Encryption
import Pdf.Toolbox.Document.Internal.Types

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

nextFreeIndex :: MonadIO m => IORef AppState -> m Int
nextFreeIndex stateRef = do
  st <- liftIO $ readIORef stateRef
  let index = stNextFree st
  liftIO $ writeIORef stateRef $ st {stNextFree = index + 1}
  return index

putPageRef :: MonadIO m => IORef AppState -> Ref -> m ()
putPageRef stateRef ref =
  liftIO $ modifyIORef stateRef $ \st -> st {stPageRefs = ref : stPageRefs st}

main :: IO ()
main = do
  files <- getArgs
  runPdfWriter Streams.stdout $ do
    writePdfHeader
    deleteObject (R 0 65535) 0
    stateRef <- liftIO $ newIORef initialAppState
    index <- nextFreeIndex stateRef
    liftIO $ modifyIORef stateRef $ \st -> st {stRootNode = R index 0}
    forM_ files $
      writePdfFile stateRef
    writeTrailer stateRef

writePdfFile :: IORef AppState -> FilePath -> PdfWriter IO ()
writePdfFile stateRef path = do
  handle <- liftIO $ openBinaryFile path ReadMode

  pdf <- liftIO $ do
    pdf <- pdfWithHandle handle

    encrypted <- isEncrypted pdf
    when encrypted $ do
      ok <- setUserPassword pdf defaultUserPassword
      unless ok $
        error "Wrong password"
    return pdf

  root <- liftIO $ document pdf >>= documentCatalog >>= catalogPageNode
  count <- liftIO $ pageNodeNKids root
  forM_ [0..count-1] $ \i -> do
    page <- liftIO $ pageNodePageByNum root i
    writePdfPage stateRef page

  liftIO $ hClose handle

writePdfPage :: IORef AppState -> Page -> PdfWriter IO ()
writePdfPage stateRef page@(Page pdf _ pageDict) = do
  parentRef <- liftIO $ stRootNode <$> readIORef stateRef
  pageIndex <- nextFreeIndex stateRef
  let pageRef = R pageIndex 0
  putPageRef stateRef pageRef
  contentRefs <- liftIO $ pageContents page
  contentRefs' <- forM contentRefs $ \r -> do
    o <- liftIO $ lookupObject pdf r
    case o of
      Stream s -> writeStream stateRef pdf s
      _ -> error "stream expected"

  resources <- do
    case HashMap.lookup "Resources" pageDict of
      Nothing -> error "No resources"
      Just v -> do
        o <- liftIO $ deref pdf v
        writeObjectChildren stateRef pdf o
  writeObject pageRef $ Dict $ HashMap.fromList [
    ("Type", Name "Page"),
    ("Contents", Array $ Vector.fromList $ map Ref contentRefs'),
    ("Resources", resources),
    ("Parent", Ref parentRef)
    ]

writeTrailer :: IORef AppState -> PdfWriter IO ()
writeTrailer stateRef = do
  pageRefs <- liftIO $ stPageRefs <$> readIORef stateRef

  rootRef <- liftIO $ stRootNode <$> readIORef stateRef
  writeObject rootRef $ Dict $ HashMap.fromList [
    ("Type", Name "Pages"),
    ("Count", Number $ fromIntegral $ length pageRefs),
    ("Kids", Array $ Vector.fromList $ map Ref $ reverse pageRefs)
    ]

  catalogIndex <- nextFreeIndex stateRef
  let catalogRef = R catalogIndex 0
  writeObject catalogRef $ Dict $ HashMap.fromList
    [ ("Type", Name "Catalog")
    , ("Pages", Ref rootRef)
    ]

  count <- liftIO $ stNextFree <$> readIORef stateRef
  writeXRefTable 0 (HashMap.fromList
    [ ("Size", Number $ fromIntegral $ count - 1)
    , ("Root", Ref catalogRef)
    ])

writeStream :: IORef AppState -> Pdf -> Stream Int64 -> PdfWriter IO Ref
writeStream stateRef pdf s@(S dict _) = do
  cont <- liftIO $ do
    S _ is <- rawStreamContent pdf s
    Lazy.ByteString.fromChunks <$> Streams.toList is

  index <- nextFreeIndex stateRef
  let ref = R index 0

  Dict dict' <- writeObjectChildren stateRef pdf (Dict dict)
  writeObject ref $ Stream $ S dict' cont
  return ref

writeObjectChildren :: IORef AppState -> Pdf -> Object () -> PdfWriter IO (Object ())
writeObjectChildren stateRef pdf (Ref r) = do
  o <- liftIO $ lookupObject pdf r
  case o of
    Stream s -> do
      ref <- writeStream stateRef pdf s
      return $ Ref ref
    _ -> do
      let o' = mapObject (error "impossible") o
      o'' <- writeObjectChildren stateRef pdf o'
      index <- nextFreeIndex stateRef
      let ref = R index 0
      writeObject ref $ mapObject (error "impossible") o''
      return $ Ref ref
writeObjectChildren stateRef pdf (Dict vals) = do
  vals' <- forM (HashMap.toList vals) $ \(key, val) -> do
    val' <- writeObjectChildren stateRef pdf val
    return (key, val')
  return $ Dict $ HashMap.fromList vals'
writeObjectChildren stateRef pdf (Array vals) = do
  vals' <- Vector.forM vals (writeObjectChildren stateRef pdf)
  return $ Array vals'
writeObjectChildren _ _ o = return o

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
