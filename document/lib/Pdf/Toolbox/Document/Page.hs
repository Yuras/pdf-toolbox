{-# LANGUAGE OverloadedStrings #-}

-- | PDF document page

module Pdf.Toolbox.Document.Page
(
  Page,
  pageParentNode,
  pageContents,
  pageMediaBox,
  pageFontDicts,
  pageExtractText,
  XObject(..),
  pageXObjects
)
where

import Data.Int
import qualified Data.Traversable as Traversable
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString
import Data.Text (Text)
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Builder as TextB
import qualified Data.Map as Map
import Control.Monad
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import Pdf.Toolbox.Core
import Pdf.Toolbox.Content

import Pdf.Toolbox.Document.Types
import Pdf.Toolbox.Document.Monad
import Pdf.Toolbox.Document.PageNode
import Pdf.Toolbox.Document.FontDict
import Pdf.Toolbox.Document.Encryption
import Pdf.Toolbox.Document.Internal.Types
import Pdf.Toolbox.Document.Internal.Util

-- | Page's parent node
pageParentNode :: MonadPdf m => Page -> PdfE m PageNode
pageParentNode (Page _ dict) = do
  ref <- lookupDict "Parent" dict >>= fromObject
  node <- loadPageNode ref
  case node of
    PageTreeNode n -> return n
    PageTreeLeaf _ -> throwE $ UnexpectedError "page parent should be a note, but leaf should"

-- | List of references to page's content streams
pageContents :: MonadPdf m => Page -> PdfE m [Ref]
pageContents page@(Page _ dict) = annotateError ("contents for page: " ++ show page) $ do
  case lookupDict' "Contents" dict of
    Nothing -> return []
    Just (ORef ref) -> do
      -- it could be reference to the only content stream,
      -- or to an array of content streams
      o <- lookupObject ref
      case o of
        OStream _ -> return [ref]
        OArray (Array objs) -> mapM fromObject objs
        _ -> throwE $ UnexpectedError $ "Unexpected value in page content ref: " ++ show o
    Just (OArray (Array objs)) -> mapM fromObject objs
    _ -> throwE $ UnexpectedError "Unexpected value in page contents"

-- | Media box, inheritable
pageMediaBox :: MonadPdf m => Page -> PdfE m (Rectangle Double)
pageMediaBox page = mediaBox (PageTreeLeaf page)

mediaBox :: MonadPdf m => PageTree -> PdfE m (Rectangle Double)
mediaBox tree = do
  let dict = case tree of
               PageTreeNode (PageNode _ d) -> d
               PageTreeLeaf (Page _ d) -> d
  case lookupDict' "MediaBox" dict of
    Just box -> fromObject box >>= rectangleFromArray
    Nothing -> do
      parent <- case tree of
                  PageTreeNode node -> do
                    parent <- pageNodeParent node
                    case parent of
                      Nothing -> throwE $ UnexpectedError $ "Media box not found"
                      Just p -> return $ PageTreeNode p
                  PageTreeLeaf page -> PageTreeNode `liftM` pageParentNode page
      mediaBox parent

-- | Font dictionaries for the page
pageFontDicts :: MonadPdf m => Page -> PdfE m [(Name, FontDict)]
pageFontDicts (Page _ dict) =
  case lookupDict' "Resources" dict of
    Nothing -> return []
    Just res -> do
      resDict <- deref res >>= fromObject
      case lookupDict' "Font" resDict of
        Nothing -> return []
        Just fonts -> do
          Dict fontsDict <- deref fonts >>= fromObject
          forM fontsDict $ \(name, font) -> do
            fontDict <- deref font >>= fromObject
            ensureType "Font" fontDict
            return (name, FontDict fontDict)

data XObject = XObject Lazy.ByteString GlyphDecoder

pageXObjects :: (MonadPdf m, MonadIO m) => Page -> PdfE m [(Name, XObject)]
pageXObjects (Page _ dict) =
  case lookupDict' "Resources" dict of
    Nothing -> return []
    Just res -> do
      resDict <- deref res >>= fromObject
      case lookupDict' "XObject" resDict of
        Nothing -> return []
        Just xo' -> do
          Dict xosDict <- deref xo' >>= fromObject
          liftM catMaybes $ forM xosDict $ \(name, o) -> do
            ref <- fromObject o
            xo <- lookupObject ref >>= toStream

            let Stream xoDict _ = xo
            case lookupDict' "Subtype" xoDict of
              Just (OName "Form") -> do
                xobject <- mkXObject xo ref
                return $ Just (name, xobject)
              _ -> return Nothing

mkXObject :: (MonadPdf m, MonadIO m) => Stream Int64 -> Ref -> PdfE m XObject
mkXObject s ref = do
  Stream dict is <- streamContent ref s
  cont <- liftIO $ Lazy.ByteString.fromChunks <$> Streams.toList is

  fontDicts <- Map.fromList `liftM` pageFontDicts (Page undefined dict)

  glyphDecoders <- Traversable.forM fontDicts $ \fontDict ->
    fontInfoDecodeGlyphs `liftM` fontDictLoadInfo fontDict
  let glyphDecoder fontId = \str ->
        case Map.lookup fontId glyphDecoders of
          Nothing -> []
          Just decode -> decode str

  return (XObject cont glyphDecoder)

-- | Extract text from the page
--
-- It tries to add spaces between chars if they don't present
-- as actual characters in content stream.
pageExtractText :: (MonadPdf m, MonadIO m) => Page -> PdfE m Text
pageExtractText page = do
  -- load fonts and create glyph decoder
  fontDicts <- Map.fromList `liftM` pageFontDicts page
  glyphDecoders <- Traversable.forM fontDicts $ \fontDict ->
    fontInfoDecodeGlyphs `liftM` fontDictLoadInfo fontDict
  let glyphDecoder fontId = \str ->
        case Map.lookup fontId glyphDecoders of
          Nothing -> []
          Just decode -> decode str

  xobjects <- Map.fromList `liftM` pageXObjects page

  -- prepare content streams
  contents <- pageContents page
  streams <- forM contents $ \ref -> do
    s@(Stream dict _) <- lookupObject ref >>= toStream
    len <- lookupDict "Length" dict >>= deref >>= fromObject >>= intValue
    return (s, ref, len)

  -- parse content streams
  maybe_decr <- getDecryptor
  let decr =
        case maybe_decr of
          Nothing -> \_ is -> return is
          Just decryptor -> \ref is -> decryptor ref DecryptStream is

  ris <- getRIS
  filters <- getStreamFilters
  is <- parseContentStream ris filters decr streams

  -- use content stream processor to extract text
  let loop s p = do
        next <- readNextOperator s
        case next of
          Just (Op_Do, [OName xoName]) -> processDo xoName p >>= loop s
          Just op -> processOp op p >>= loop s
          Nothing -> return p

      processDo name p = do
        case Map.lookup name xobjects of
          Nothing -> return p
          Just (XObject cont gdec) -> do
            xois <- liftIO $ do
              cont_is <- Streams.fromLazyByteString cont
              Streams.parserToInputStream parseContent cont_is
            let gdec' = prGlyphDecoder p
            p' <- loop xois (p {prGlyphDecoder = gdec})
            return (p' {prGlyphDecoder = gdec'})

  p <- loop is $ mkProcessor {
    prGlyphDecoder = glyphDecoder
    }

  return $ glyphsToText (prGlyphs p)

-- | Convert glyphs to text, trying to add spaces and newlines
--
-- It takes list of spans. Each span is a list of glyphs that are outputed in one shot.
-- So we don't need to add space inside span, only between them.
glyphsToText :: [[Glyph]] -> Text
glyphsToText = TextL.toStrict . TextB.toLazyText . snd . foldl step ((Vector 0 0, False), mempty)
  where
  step acc [] = acc
  step ((Vector lx2 ly2, wasSpace), res) sp =
    let Vector x1 y1 = glyphTopLeft (head sp)
        Vector x2 _ = glyphBottomRight (last sp)
        Vector _ y2 = glyphTopLeft (last sp)
        space =
          if abs (ly2 - y1) < 1.8
            then  if wasSpace || abs (lx2 - x1) < 1.8
                    then mempty
                    else TextB.singleton ' '
            else TextB.singleton '\n'
        txt = TextB.fromLazyText $ TextL.fromChunks $ mapMaybe glyphText sp
        endWithSpace = glyphText (last sp) == Just " "
    in ((Vector x2 y2, endWithSpace), mconcat [res, space, txt])
