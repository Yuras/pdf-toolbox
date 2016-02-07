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
  pageExtractGlyphs,
  glyphsToText
)
where

import Data.Maybe
import qualified Data.List as List
import qualified Data.Traversable as Traversable
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy.Text
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import Pdf.Toolbox.Core
import Pdf.Toolbox.Core.Util
import Pdf.Toolbox.Core.Name (Name)
import Pdf.Toolbox.Content

import Pdf.Toolbox.Document.Pdf
import Pdf.Toolbox.Document.Types
import Pdf.Toolbox.Document.PageNode
import Pdf.Toolbox.Document.FontDict
import Pdf.Toolbox.Document.Internal.Types
import Pdf.Toolbox.Document.Internal.Util

-- | Page's parent node
pageParentNode :: Page -> IO PageNode
pageParentNode (Page pdf _ dict) = do
  ref <- sure $ (HashMap.lookup "Parent" dict >>= refValue)
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
  case HashMap.lookup "Contents" dict of
    Nothing -> return []
    Just (Ref ref) -> do
      -- it could be reference to the only content stream,
      -- or to an array of content streams
      o <- lookupObject pdf ref >>= deref pdf
      case o of
        Stream _ -> return [ref]
        Array objs -> forM (Vector.toList objs) $ \obj ->
          sure $ refValue obj `notice` "Content should be a reference"
        _ -> throw $ Corrupted
          ("Unexpected value in page content ref: " ++ show o) []
    Just (Array objs) -> forM (Vector.toList objs) $ \obj ->
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
  case HashMap.lookup "MediaBox" dict of
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
  case HashMap.lookup "Resources" dict of
    Nothing -> return []
    Just res -> do
      res' <- deref pdf res
      resDict <- sure $ dictValue res'
          `notice` "Resources should be a dictionary"
      case HashMap.lookup "Font" resDict of
        Nothing -> return []
        Just fonts -> do
          fonts' <- deref pdf fonts
          fontsDict <- sure $ dictValue fonts'
              `notice` "Font should be a dictionary"
          forM (HashMap.toList fontsDict) $ \(name, font) -> do
            font' <- deref pdf font
            fontDict <- sure $ dictValue font'
                `notice` "Each font should be a dictionary"
            ensureType "Font" fontDict
            return (name, FontDict pdf fontDict)

data XObject = XObject Lazy.ByteString GlyphDecoder

pageXObjects :: Page -> IO [(Name, XObject)]
pageXObjects (Page pdf _ dict) =
  case HashMap.lookup "Resources" dict of
    Nothing -> return []
    Just res -> do
      resDict <- do
        v <- deref pdf res
        sure $ dictValue v
          `notice` "Resources should be a dict"

      case HashMap.lookup "XObject" resDict of
        Nothing -> return []
        Just xo -> do
          xosDict <- do
            v <- deref pdf xo
            sure $ dictValue v
              `notice` "XObject should be a dict"
          result <- forM (HashMap.toList xosDict) $ \(name, o) -> do
            ref <- sure $ refValue o
              `notice` "Not a ref"
            s@(S xoDict _) <- do
              v <- lookupObject pdf ref
              sure $ streamValue v
                `notice` "Not a stream"

            case HashMap.lookup "Subtype" xoDict of
              Just (Name "Form") -> do
                is <- streamContent pdf ref s
                cont <- Lazy.ByteString.fromChunks <$> Streams.toList is

                fontDicts <- Map.fromList <$>
                  pageFontDicts (Page pdf undefined xoDict)

                glyphDecoders <- Traversable.forM fontDicts $ \fontDict ->
                  fontInfoDecodeGlyphs <$> fontDictLoadInfo fontDict
                let glyphDecoder fontName = \str ->
                      case Map.lookup fontName glyphDecoders of
                        Nothing -> []
                        Just decode -> decode str

                return (name, Just (XObject cont glyphDecoder))

              _ -> return (name, Nothing)

          let step l (_, Nothing) = l
              step l (name, Just o) = (name, o) : l
          return (List.foldl' step [] result)

-- | Extract text from the page
--
-- It tries to add spaces between chars if they don't present
-- as actual characters in content stream.
pageExtractText :: Page -> IO Text
pageExtractText page = glyphsToText <$> pageExtractGlyphs page

pageExtractGlyphs :: Page -> IO [[Glyph]]
pageExtractGlyphs page = do
  fontDicts <- Map.fromList <$> pageFontDicts page
  glyphDecoders <- Traversable.forM fontDicts $ \fontDict ->
    fontInfoDecodeGlyphs <$> fontDictLoadInfo fontDict
  let glyphDecoder fontName = \str ->
        case Map.lookup fontName glyphDecoders of
          Nothing -> []
          Just decode -> decode str

  xobjects <- Map.fromList <$> pageXObjects page

  is <- do
    contents <- pageContents page
    let Page pdf _ _ = page
    is <- combinedContent pdf contents
    Streams.parserToInputStream parseContent is

  -- use content stream processor to extract text
  let loop s p = do
        next <- readNextOperator s
        case next of
          Just (Op_Do, [Name name]) -> processDo name p >>= loop s
          Just op ->
            case processOp op p of
              Left err -> throwIO (Unexpected err [])
              Right  p' -> loop s p'
          Nothing -> return p

      processDo name p = do
        case Map.lookup name xobjects of
          Nothing -> return p
          Just (XObject cont gdec) -> do
            s <- do
              s <- Streams.fromLazyByteString cont
              Streams.parserToInputStream parseContent s

            let gdec' = prGlyphDecoder p
            p' <- loop s (p {prGlyphDecoder = gdec})
            return (p' {prGlyphDecoder = gdec'})

  p <- loop is $ mkProcessor {
    prGlyphDecoder = glyphDecoder
    }
  return (prGlyphs p)

combinedContent :: Pdf -> [Ref] -> IO (InputStream ByteString)
combinedContent pdf refs = do
  allStreams <- forM refs $ \ref -> do
    o <- lookupObject pdf ref
    case o of
      Stream s -> return (ref, s)
      _ -> throwIO (Corrupted "Page content is not a stream" [])

  Streams.fromGenerator $ forM_ allStreams $ \(ref, stream) -> do
    is <- liftIO $ streamContent pdf ref stream
    yield is
  where
  yield is =
    liftIO (Streams.read is)
    >>= maybe (return ()) (\c -> Streams.yield c >> yield is)

-- | Convert glyphs to text, trying to add spaces and newlines
--
-- It takes list of spans. Each span is a list of glyphs that are outputed in one shot.
-- So we don't need to add space inside span, only between them.
glyphsToText :: [[Glyph]] -> Text
glyphsToText = Lazy.Text.toStrict . Text.Builder.toLazyText . snd . foldl step ((Vector 0 0, False), mempty)
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
                    else Text.Builder.singleton ' '
            else Text.Builder.singleton '\n'
        txt = Text.Builder.fromLazyText $ Lazy.Text.fromChunks $ mapMaybe glyphText sp
        endWithSpace = glyphText (last sp) == Just " "
    in ((Vector x2 y2, endWithSpace), mconcat [res, space, txt])
