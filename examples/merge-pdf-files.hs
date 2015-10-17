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
import qualified Data.ByteString.Lazy as Lazy.ByteString
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
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

nextFreeIndex :: Monad m => StateT AppState m Int
nextFreeIndex = do
  st <- get
  let index = stNextFree st
  put $ st {stNextFree = index + 1}
  return index

putPageRef :: Monad m => Ref -> StateT AppState m ()
putPageRef ref =
  modify $ \st -> st {stPageRefs = ref : stPageRefs st}

main :: IO ()
main = do
  files <- getArgs
  runPdfWriter Streams.stdout $ do
    writePdfHeader
    deleteObject (R 0 65535) 0
    flip evalStateT initialAppState $ do
      index <- nextFreeIndex
      modify $ \st -> st {stRootNode = R index 0}
      forM_ files writePdfFile
      writeTrailer

writePdfFile :: FilePath -> StateT AppState (PdfWriter IO) ()
writePdfFile path = do
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
    writePdfPage page
  {-
  runPdfWithHandle handle knownFilters $ do
    encrypted <- isEncrypted
    when encrypted $ do
      ok <- setUserPassword defaultUserPassord
      unless ok $
        error "Wrong password"

    root <- document >>= documentCatalog >>= catalogPageNode
    count <- pageNodeNKids root
    forM_ [0..count-1] $ \i -> do
      page <- pageNodePageByNum root i
      writePdfPage page
      -}

  liftIO $ hClose handle

writePdfPage :: Page -> StateT AppState (PdfWriter IO) ()
writePdfPage page@(Page pdf _ pageDict) = do
  parentRef <- gets stRootNode
  pageIndex <- nextFreeIndex
  let pageRef = R pageIndex 0
  putPageRef pageRef
  contentRefs <- liftIO $ pageContents page
  contentRefs' <- forM contentRefs $ \r -> do
    o <- liftIO $ lookupObject pdf r
    case o of
      Stream s -> writeStream pdf s
      _ -> error "stream expected"

  resources <- do
    case HashMap.lookup "Resources" pageDict of
      Nothing -> error "No resources"
      Just v -> do
        o <- liftIO $ deref pdf v
        writeObjectChildren pdf o
  lift $ writeObject pageRef $ Dict $ HashMap.fromList [
    ("Type", Name "Page"),
    ("Contents", Array $ Vector.fromList $ map Ref contentRefs'),
    ("Resources", resources),
    ("Parent", Ref parentRef)
    ]

writeTrailer :: StateT AppState (PdfWriter IO) ()
writeTrailer = do
  pageRefs <- gets stPageRefs

  rootRef <- gets stRootNode
  lift $ writeObject rootRef $ Dict $ HashMap.fromList [
    ("Type", Name "Pages"),
    ("Count", Number $ fromIntegral $ length pageRefs),
    ("Kids", Array $ Vector.fromList $ map Ref $ reverse pageRefs)
    ]

  catalogIndex <- nextFreeIndex
  let catalogRef = R catalogIndex 0
  lift $ writeObject catalogRef $ Dict $ HashMap.fromList
    [ ("Type", Name "Catalog")
    , ("Pages", Ref rootRef)
    ]

  count <- gets stNextFree
  lift $ writeXRefTable 0 (HashMap.fromList
    [ ("Size", Number $ fromIntegral $ count - 1)
    , ("Root", Ref catalogRef)
    ])

writeStream :: Pdf -> Stream Int64 -> StateT AppState (PdfWriter IO) Ref
writeStream pdf s@(S dict _) = do
  cont <- liftIO $ do
    S _ is <- rawStreamContent pdf s
    Lazy.ByteString.fromChunks <$> Streams.toList is

  index <- nextFreeIndex
  let ref = R index 0

  Dict dict' <- writeObjectChildren pdf (Dict dict)
  lift $ writeObject ref $ Stream $ S dict' cont
  return ref

writeObjectChildren :: Pdf -> Object () -> StateT AppState (PdfWriter IO) (Object ())
writeObjectChildren pdf (Ref r) = do
  o <- liftIO $ lookupObject pdf r
  case o of
    Stream s -> do
      ref <- writeStream pdf s
      return $ Ref ref
    _ -> do
      let o' = mapObject (error "impossible") o
      o'' <- writeObjectChildren pdf o'
      index <- nextFreeIndex
      let ref = R index 0
      lift $ writeObject ref $ mapObject (error "impossible") o''
      return $ Ref ref
writeObjectChildren pdf (Dict vals) = do
  vals' <- forM (HashMap.toList vals) $ \(key, val) -> do
    val' <- writeObjectChildren pdf val
    return (key, val')
  return $ Dict $ HashMap.fromList vals'
writeObjectChildren pdf (Array vals) = do
  vals' <- Vector.forM vals (writeObjectChildren pdf)
  return $ Array vals'
writeObjectChildren _ o = return o

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
