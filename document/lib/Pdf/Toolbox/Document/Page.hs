{-# LANGUAGE OverloadedStrings #-}

-- | PDF document page

module Pdf.Toolbox.Document.Page
(
  Page,
  pageParentNode,
  pageContents,
  pageMediaBox,
  pageFontDicts,
  --pageExtractText,
)
where

import Data.Functor
import Control.Monad
import Control.Exception

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Util

import Pdf.Toolbox.Document.Pdf
import Pdf.Toolbox.Document.Types
import Pdf.Toolbox.Document.PageNode
import Pdf.Toolbox.Document.FontDict
import Pdf.Toolbox.Document.Internal.Types
import Pdf.Toolbox.Document.Internal.Util

-- | Page's parent node
pageParentNode :: Page -> IO PageNode
pageParentNode (Page pdf _ dict) = do
  ref <- sure $ (lookupDict "Parent" dict >>= refValue)
      `notice` "Parent should be a reference"
  node <- loadPageNode pdf ref
  case node of
    PageTreeNode n -> return n
    PageTreeLeaf _ -> throw $ Corrupted
      "page parent should be a note, but leaf found" []

-- | List of references to page's content streams
pageContents :: Page -> IO [Ref]
pageContents (Page pdf pageRef dict) =
  message ("contents for page: " ++ show pageRef) $ do
  case lookupDict "Contents" dict of
    Nothing -> return []
    Just (ORef ref) -> do
      -- it could be reference to the only content stream,
      -- or to an array of content streams
      o <- lookupObject pdf ref >>= deref pdf
      case o of
        OStream _ -> return [ref]
        OArray (Array objs) -> forM objs $ \obj ->
          sure $ refValue obj `notice` "Content should be a reference"
        _ -> throw $ Corrupted
          ("Unexpected value in page content ref: " ++ show o) []
    Just (OArray (Array objs)) -> forM objs $ \obj ->
      sure $ refValue obj `notice` "Content should be a reference"
    _ -> throw $ Corrupted "Unexpected value in page contents" []

-- | Media box, inheritable
pageMediaBox :: Page -> IO (Rectangle Double)
pageMediaBox page = mediaBoxRec (PageTreeLeaf page)

mediaBoxRec :: PageTree -> IO (Rectangle Double)
mediaBoxRec tree = do
  let (pdf, dict) =
        case tree of
          PageTreeNode (PageNode p _ d) -> (p, d)
          PageTreeLeaf (Page p _ d) -> (p, d)
  case lookupDict "MediaBox" dict of
    Just box -> do
      box' <- deref pdf box
      arr <- sure $ arrayValue box'
          `notice` "MediaBox should be an array"
      sure $ rectangleFromArray arr
    Nothing -> do
      parent <-
        case tree of
          PageTreeNode node -> do
            parent <- pageNodeParent node
            case parent of
              Nothing -> throw $ Corrupted "Media box not found" []
              Just p -> return (PageTreeNode p)
          PageTreeLeaf page -> PageTreeNode <$> pageParentNode page
      mediaBoxRec parent

-- | Font dictionaries for the page
pageFontDicts :: Page -> IO [(Name, FontDict)]
pageFontDicts (Page pdf _ dict) =
  case lookupDict "Resources" dict of
    Nothing -> return []
    Just res -> do
      res' <- deref pdf res
      resDict <- sure $ dictValue res'
          `notice` "Resources should be a dictionary"
      case lookupDict "Font" resDict of
        Nothing -> return []
        Just fonts -> do
          fonts' <- deref pdf fonts
          Dict fontsDict <- sure $ dictValue fonts'
              `notice` "Font should be a dictionary"
          forM fontsDict $ \(name, font) -> do
            font' <- deref pdf font
            fontDict <- sure $ dictValue font'
                `notice` "Each font should be a dictionary"
            ensureType "Font" fontDict
            return (name, FontDict pdf fontDict)

{-

-- | Extract text from the page
--
-- It tries to add spaces between chars if they don't present
-- as actual characters in content stream.
pageExtractText :: (MonadPdf m, MonadIO m) => Page -> PdfE m Text
pageExtractText page = do
  -- load fonts and create glyph decoder
  fontDicts <- Map.fromList <$> pageFontDicts page
  glyphDecoders <- Traversable.forM fontDicts $ \fontDict ->
    fontInfoDecodeGlyphs <$> fontDictLoadInfo fontDict
  let glyphDecoder fontName = \str ->
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
          Just op -> processOp op p >>= loop
          Nothing -> return $ glyphsToText (prGlyphs p)
  loop $ mkProcessor {
    prGlyphDecoder = glyphDecoder
    }

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
-}
