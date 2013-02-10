{-# LANGUAGE OverloadedStrings #-}

-- | Cross reference

module Pdf.Toolbox.XRef
(
  XRef(..),
  XRefEntry(..),
  TableEntry(..),
  StreamEntry(..),
  lastXRef,
  prevXRef,
  trailer,
  lookupEntry,
  lookupEntry'
)
where

import Data.Int
import qualified Data.ByteString as BS
import Control.Error
import Control.Monad

import Pdf.Toolbox.Object.Types
import Pdf.Toolbox.Object.Util
import Pdf.Toolbox.IO
import Pdf.Toolbox.Parsers.XRef
import Pdf.Toolbox.Stream
import Pdf.Toolbox.Error

-- | Entry in cross reference table
data TableEntry = TableEntry {
  teOffset :: Int64,
  teGen :: Int,
  teIsFree :: Bool
  } deriving Show

-- | Entry in cross reference stream
data StreamEntry =
  -- | Object number and generation
  StreamEntryFree Int Int |
  -- | Object offset (in bytes from the beginning of file) and generation
  StreamEntryUsed Int64 Int |
  -- | Object number of object stream and index within the object stream
  StreamEntryCompressed Int Int
  deriving Show

-- | Entry in cross reference
data XRefEntry =
  XRefTableEntry TableEntry |
  XRefStreamEntry StreamEntry
  deriving Show

-- | Cross reference
data XRef =
  -- | Offset
  XRefTable Int64 |
  -- | Stream with decoded content
  XRefStream (Stream Int64)
  deriving Show

-- | Find the last cross reference
lastXRef :: MonadIO m => RIS -> PdfE m XRef
lastXRef ris = annotateError "Can't find the last xref" $ do
  sz <- size ris
  seek ris $ max 0 (sz - 1024)
  off <- inputStream ris >>= parse startXRef
  readXRef ris off

readXRef :: MonadIO m => RIS -> Int64 -> PdfE m XRef
readXRef ris off = do
  seek ris off
  table <- inputStream ris >>= isTable
  if table
    then return $ XRefTable off
    else XRefStream `liftM` readStream ris

isTable :: MonadIO m => IS -> PdfE m Bool
isTable is = do
  res <- runEitherT (parse tableXRef is)
  case res of
    Right _ -> return True
    Left _ -> return False

-- | Find prev cross reference
prevXRef :: MonadIO m => RIS -> XRef -> PdfE m (Maybe XRef)
prevXRef ris xref = annotateError "Can't find prev xref" $ do
  tr <- trailer ris xref
  prev <- runEitherT $ lookupDict "Prev" tr
  case prev of
    Right p -> do
      off <- fromObject p >>= intValue
      Just `liftM` readXRef ris (fromIntegral off)
    Left _ -> return Nothing

-- | Read trailer for the xref
trailer :: MonadIO m => RIS -> XRef -> PdfE m Dict
trailer ris (XRefTable off) = annotateError ("Reading trailer for xref table: " ++ show off) $ do
  seek ris off
  inputStream ris >>= \is -> do
    _ <- isTable is
    skipTable is
    parse parseTrailerAfterTable is
trailer _ (XRefStream (Stream dict _)) = return dict

skipTable :: MonadIO m => IS -> PdfE m ()
skipTable is =
  subsectionHeader is >>= go . snd
  where
  go count = nextSubsectionHeader is count >>= maybe (return ()) (go . snd)

subsectionHeader :: MonadIO m => IS -> PdfE m (Int, Int)
subsectionHeader = parse parseSubsectionHeader

nextSubsectionHeader :: MonadIO m => IS -> Int -> PdfE m (Maybe (Int, Int))
nextSubsectionHeader is count = do
  skipSubsection is count
  hush `liftM` (runEitherT $ subsectionHeader is)

skipSubsection :: MonadIO m => IS -> Int -> PdfE m ()
skipSubsection is count = dropExactly (count * 20) is

-- | Find xref entity for ref
lookupEntry :: MonadIO m => RIS -> [StreamFilter] -> Ref -> PdfE m XRefEntry
lookupEntry ris filters ref = annotateError ("Can't find entity for ref: " ++ show ref) $
  lastXRef ris >>= loop
  where
  loop xref = do
    res <- lookupEntry' ris filters ref xref
    case res of
      Just e -> return e
      Nothing -> do
        prev <- prevXRef ris xref
        case prev of
          Nothing -> left $ UnexpectedError "There are no more xrefs"
          Just xref' -> loop xref'

-- | Find entity for ref in the specified xref
lookupEntry' :: MonadIO m => RIS -> [StreamFilter] -> Ref -> XRef -> PdfE m (Maybe XRefEntry)
lookupEntry' ris filters ref xref =
  annotateError ("Can't find entry in xref " ++ show xref ++ " for ref " ++ show ref) $
    lookup'
  where
  lookup' =
    case xref of
      (XRefTable off) -> do
        seek ris off
        _ <- inputStream ris >>= isTable
        fmap XRefTableEntry `liftM` readTableEntry ris ref
      (XRefStream s) -> do
        decoded <- streamContent ris filters s
        fmap XRefStreamEntry `liftM` readStreamEntry decoded ref

readTableEntry :: MonadIO m => RIS -> Ref -> PdfE m (Maybe TableEntry)
readTableEntry ris (Ref index gen) = annotateError "Can't read entry from xref table" $
  inputStream ris >>= subsectionHeader >>= go
  where
  go (start, count) = do
    if index >= start && index < start + count
      then do
        tell ris >>= seek ris . (+ (fromIntegral $ index - start) * 20)
        (off, gen', free) <- inputStream ris >>= parse parseTableEntry
        unless (gen == gen') $ left $ UnexpectedError "Generation mismatch"
        return $ Just $ TableEntry off gen free
      else do
        is <- inputStream ris
        nextSubsectionHeader is count >>= maybe (return Nothing) go

-- See pdf1.7 spec: 7.5.8 Cross-Reference Streams
readStreamEntry :: MonadIO m => Stream IS -> Ref -> PdfE m (Maybe StreamEntry)
readStreamEntry (Stream dict is) (Ref objNumber _) = annotateError "Can't parse xref stream" $ do
  sz <- lookupDict "Size" dict >>= fromObject >>= intValue

  index <- do
    Array i <- (lookupDict "Index" dict >>= fromObject)
      `catchT`
      const (return $ Array [ONumber $ NumInt 0, ONumber $ NumInt sz])
    let convertIndex res [] = return $ reverse res
        convertIndex res (x1:x2:xs) = do
          from <- fromObject x1 >>= intValue
          count <- fromObject x2 >>= intValue
          convertIndex ((from, count) : res) xs
        convertIndex _ _ = left $ UnexpectedError $ "Malformed Index in xref stream: " ++ show i
    convertIndex [] i

  width <- do
    Array w <- lookupDict "W" dict >>= fromObject
    mapM (fromObject >=> intValue) w
  unless (length width == 3) $ left $ UnexpectedError $ "Malformed With array in xref stream: " ++ show width

  values <- do
    let position = loop 0 index
        totalWidth = sum width
        loop _ [] = Nothing
        loop pos ((from, count) : xs) =
          if objNumber < from || objNumber >= from + count
            then loop (pos + totalWidth * count) xs
            else Just (pos + totalWidth * (objNumber - from))
    case position of
      Nothing -> return Nothing
      Just p -> dropExactly p is >> (Just . BS.unpack) `liftM` readExactly totalWidth is

  case values of
    Nothing -> return Nothing
    Just vs -> do
      let [v1, v2, v3] = map conv $ collect [] width vs :: [Int64]
            where
            conv l = conv' (length l - 1) 0 l
            conv' _ res [] = res
            conv' power res (x:xs) = conv' (power-1) (res + (fromIntegral x * 256 ^ power)) xs
            collect res [] [] = reverse res
            collect res (x:xs) ys = collect (take x ys : res) xs (drop x ys)
            collect _ _ _ = error "readStreamEntry: collect: impossible"
      case v1 of
        0 -> return $ Just $ StreamEntryFree (fromIntegral v2) (fromIntegral v3)
        1 -> return $ Just $ StreamEntryUsed v2 (fromIntegral v3)
        2 -> return $ Just $ StreamEntryCompressed (fromIntegral v2) (fromIntegral v3)
        _ -> left $ UnexpectedError $ "Unexpected xret stream entry type: " ++ show v1
