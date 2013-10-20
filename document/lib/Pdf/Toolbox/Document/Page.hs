{-# LANGUAGE OverloadedStrings #-}

-- | PDF document page

module Pdf.Toolbox.Document.Page
(
  Page,
  pageParentNode,
  pageContents,
  pageMediaBox,
  pageFontDicts,
  pageExtractText
)
where

import Data.Functor
import qualified Data.Traversable as Traversable
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import Control.Monad
import qualified System.IO.Streams as Streams

import Pdf.Toolbox.Core
import Pdf.Toolbox.Content

import Pdf.Toolbox.Document.Types
import Pdf.Toolbox.Document.Monad
import Pdf.Toolbox.Document.PageNode
import Pdf.Toolbox.Document.Internal.Types
import Pdf.Toolbox.Document.Internal.Util

-- | Page's parent node
pageParentNode :: MonadPdf m => Page -> PdfE m PageNode
pageParentNode (Page _ dict) = do
  ref <- lookupDict "Parent" dict >>= fromObject
  node <- loadPageNode ref
  case node of
    PageTreeNode n -> return n
    PageTreeLeaf _ -> left $ UnexpectedError "page parent should be a note, but leaf should"

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
        _ -> left $ UnexpectedError $ "Unexpected value in page content ref: " ++ show o
    Just (OArray (Array objs)) -> mapM fromObject objs
    _ -> left $ UnexpectedError "Unexpected value in page contents"

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
                      Nothing -> left $ UnexpectedError $ "Media box not found"
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

-- | Extract text from the page
--
-- Right now it doesn't even try to insert additional spaces or newlines,
-- and returns text as it is embeded. But someday it will.
pageExtractText :: (MonadPdf m, MonadIO m) => Page -> PdfE m Text
pageExtractText page = do
  -- collect unicode cmaps to be able to decode glyphs
  fontDicts <- Map.fromList <$> pageFontDicts page
  glyphDecoders <- Traversable.forM fontDicts $ \(FontDict fontDict) ->
    -- Note: we are not interested in glyph positions (at least for now)
    -- so lets fake bounding box and width
    case lookupDict' "ToUnicode" fontDict of
      Nothing -> return $ \(Str str) -> flip map (BS8.unpack str) $ \c ->
        -- Treat each Word8 as ASCII char
        -- XXX: That is wrong, need to handle encoding...
        (Glyph {
          glyphCode = BS8.pack [c],
          glyphTopLeft = Vector 0 0,
          glyphBottomRight = Vector 1 1,
          glyphText = Just $ Text.pack [c]
          }, 1)
      Just o -> do
        -- Ok, we have cmap. Lets parse it
        ref <- fromObject o
        toUnicode <- lookupObject ref
        case toUnicode of
          OStream s -> do
            Stream _ is <- streamContent ref s
            content <- BS.concat <$> liftIO (Streams.toList is)
            cmap <-
              case parseUnicodeCMap content of
                Left e -> left $ UnexpectedError $ "Can't parse cmap: " ++ show e
                Right v -> return v
            return $ (map $ \g -> (g, 1)) . cmapDecodeString cmap
          _ -> left $ UnexpectedError "ToUnicode: not a stream"
  let glyphDecoder = \fontName str ->
        case Map.lookup fontName glyphDecoders of
          Nothing -> []
          Just decode -> decode str

  -- prepare content streams
  contents <- pageContents page
  streams <- forM contents $ \ref -> do
    s@(Stream dict _) <- lookupObject ref >>= toStream
    len <- lookupDict "Length" dict >>= deref >>= fromObject >>= intValue
    return (s, ref, len)

  -- parse content streams
  decryptor <- fromMaybe (const return) <$> getDecryptor

  ris <- getRIS
  filters <- getStreamFilters
  is <- parseContentStream ris filters decryptor streams

  -- use content stream processor to extract text
  let loop p = do
        next <- readNextOperator is
        case next of
          Nothing -> return $ Text.concat $ mapMaybe glyphText $ concat $ prGlyphs p
          Just op -> processOp op p >>= loop
  loop $ mkProcessor {
    prGlyphDecoder = glyphDecoder
    }

  where
  cmapDecodeString :: UnicodeCMap -> Str -> [Glyph]
  cmapDecodeString cmap (Str str) = go str
    where
    go s =
      case unicodeCMapNextGlyph cmap s of
        Nothing -> []
        Just (g, rest) ->
          let glyph = Glyph {
            glyphCode = g,
            glyphTopLeft = Vector 0 0,
            glyphBottomRight = Vector 1 1,
            glyphText = unicodeCMapDecodeGlyph cmap g
            }
          in glyph : go rest
