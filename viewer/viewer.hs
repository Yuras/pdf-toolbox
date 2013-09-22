
module Main
(
  main
)
where

import Data.Monoid
import Data.Maybe
import Data.String
import Data.Functor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextE
import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import System.IO
import qualified System.IO.Streams as Streams
import Graphics.UI.Gtk hiding (Rectangle)
import Graphics.Rendering.Cairo hiding (transform)

import Pdf.Toolbox.Document
import Pdf.Toolbox.Document.Internal.Types
import Pdf.Toolbox.Content.Ops
import Pdf.Toolbox.Content.Parser
import Pdf.Toolbox.Content.Processor
import Pdf.Toolbox.Content.Transform
import Pdf.Toolbox.Content.UnicodeCMap

main :: IO ()
main = do
  [file] <- initGUI
  mvar <- newEmptyMVar
  withBinaryFile file ReadMode $ \h -> do
  _ <- forkIO $ pdfThread h mvar

  (rootNode, totalPages, title) <- pdfSync mvar $ do
    encrypted <- isEncrypted
    when encrypted $ do
      liftIO $ print "WARNING: Document is encrypted, it is not fully supported yet"
      ok <- setUserPassword defaultUserPassword
      unless ok $ error "Need user password"
    pdf <- document
    title <- do
      info <- documentInfo pdf
      case info of
        Nothing -> return Nothing
        Just i -> infoTitle i
    root <- documentCatalog pdf >>= catalogPageNode
    total <- pageNodeNKids root
    return (root, total, title)

  firstPage <- pdfSync mvar $ do
    pageNodePageByNum rootNode 0

  page <- newIORef (firstPage, 0)

  let winTitle = maybe "Untitled" (\(Str s) -> BS8.unpack s) title

  window <- windowNew
  set window [
    windowDefaultWidth := 300,
    windowDefaultHeight := 300,
    windowTitle := winTitle
    ]
  _ <- on window deleteEvent $ liftIO mainQuit >> return True

  vbox <- vBoxNew False 10
  containerAdd window vbox

  hbuttonBox <- hButtonBoxNew
  boxPackStart vbox hbuttonBox PackNatural 0

  prevButton <- buttonNewWithLabel "Prev"
  boxPackStart hbuttonBox prevButton PackNatural 0

  nextButton <- buttonNewWithLabel "Next"
  boxPackStart hbuttonBox nextButton PackNatural 0

  frame <- frameNew
  boxPackStart vbox frame PackGrow 0

  canvas <- drawingAreaNew
  containerAdd frame canvas

  _ <- on prevButton buttonActivated $ do
    (_, num) <- readIORef page
    when (num > 0) $ do
      p <- pdfSync mvar $ pageNodePageByNum rootNode (num - 1)
      writeIORef page (p, num - 1)
      widgetQueueDraw canvas
  _ <- on nextButton buttonActivated $ do
    (_, num) <- readIORef page
    when (num < totalPages - 1) $ do
      p <- pdfSync mvar $ pageNodePageByNum rootNode (num + 1)
      writeIORef page (p, num + 1)
      widgetQueueDraw canvas

  widgetShowAll window
  draw <- widgetGetDrawWindow canvas
  _ <- on canvas exposeEvent $ do
    liftIO $ renderWithDrawable draw $ onDraw mvar page
    return True

  mainGUI

onDraw :: MVar (Pdf IO Bool) -> IORef (Page, Int) -> Render ()
onDraw mvar page = do
  setSourceRGB 1 1 1
  setLineWidth 1

  (pg, _) <- liftIO $ readIORef page
  Rectangle llx lly urx ury <- liftIO $ pdfSync mvar $ pageMediaBox pg

  chan <- liftIO $ startRender mvar pg

  moveTo llx lly
  lineTo llx ury
  lineTo urx ury
  lineTo urx lly
  lineTo llx lly
  closePath
  fill

  setSourceRGB 0 0 0
  let loop = do
        cmd <- liftIO $ readChan chan
        case cmd of
          Nothing -> return ()
          Just (Vector x y, str) -> do
            moveTo x (ury - y)
            showText str
            stroke
            loop
  loop

startRender :: MVar (Pdf IO Bool) -> Page -> IO (Chan (Maybe (Vector Double, String)))
startRender mvar page = do
  chan <- newChan
  putMVar mvar $ do
    fontDicts <- pageFontDicts page

    -- Collect unicode cmaps for all fonts
    cmaps <- forM fontDicts $ \(name, FontDict dict) -> do
      case lookupDict' (fromString "ToUnicode") dict of
        Nothing -> return (name, Nothing)
        Just o -> do
          ref <- fromObject o
          toUnicode <- lookupObject ref
          case toUnicode of
            OStream s -> do
              Stream _ is <- streamContent ref s
              content <- BS.concat <$> liftIO (Streams.toList is)
              cmap <- case parseUnicodeCMap content of
                        Left _err -> error $ "can't parse cmap: " ++ _err
                        Right cmap -> return $ Just cmap
              return (name, cmap)
            _ -> left $ UnexpectedError "ToUnicode: not a stream"

    contents <- pageContents page
    streams <- forM contents $ \ref -> do
      s@(Stream dict _) <- lookupObject ref >>= toStream
      len <- lookupDict (fromString "Length") dict >>= deref >>= fromObject >>= intValue
      return (s, ref, len)
    ris <- getRIS
    decryptor <- do
      dec <-getDecryptor
      case dec of
        Nothing -> return (const return)
        Just d -> return d
    is <- parseContentStream ris knownFilters decryptor streams

    -- Convert bytestring to text via unicode cmap
    let cmapDecode cmap str = mconcat $ cmapDecode' cmap str
        cmapDecode' cmap str =
          case unicodeCMapNextGlyph cmap str of
            Just (glyph, rest) ->
              case unicodeCMapDecodeGlyph cmap glyph of
                Nothing -> cmapDecode' cmap rest
                Just txt -> txt : cmapDecode' cmap rest
            _ -> []

    let loop p = do
          next <- readNextOperator is
          case next of
            Nothing -> return ()
            Just op -> do
              case op of
                (Op_Tj, [OStr (Str str)]) -> do
                  let gstate = prState p
                      tm = gsTextMatrix gstate
                      ctm = gsCurrentTransformMatrix gstate
                      pos = transform (multiply tm ctm) (Vector 0 0)
                      fontName = fromMaybe mempty $ gsFont gstate
                      mcmap = lookup (Name fontName) cmaps
                      str' = case mcmap of
                               Just (Just cmap) -> cmapDecode cmap str
                               _ -> Text.decodeUtf8With (TextE.replace '?') str
                  liftIO $ writeChan chan $ Just (pos, Text.unpack str')
                (Op_TJ, [OArray (Array array)]) -> do
                  let gstate = prState p
                      tm = gsTextMatrix gstate
                      ctm = gsCurrentTransformMatrix gstate
                      pos = transform (multiply tm ctm) (Vector 0 0)
                      fontName = fromMaybe mempty $ gsFont gstate
                      mcmap = lookup (Name fontName) cmaps
                      toS (OStr (Str s)) =
                        case mcmap of
                          Just (Just cmap) -> Text.unpack $ cmapDecode cmap s
                          _ -> BS8.unpack s
                      toS _ = ""
                  liftIO $ writeChan chan $ Just (pos, concatMap toS array)
                _ -> return ()
              processOp op p >>= loop
    loop mkProcessor
    liftIO $ writeChan chan Nothing
    return False
  return chan

pdfThread :: Handle -> MVar (Pdf IO Bool) -> IO ()
pdfThread handle mvar = do
  res <- runPdfWithHandle handle knownFilters loop
  print res
  where
  loop = do
    action <- liftIO $ takeMVar mvar
    exit <- action
    unless exit loop

pdfSync :: MVar (Pdf IO Bool) -> Pdf IO a -> IO a
pdfSync mvar action = do
  mvar' <- newEmptyMVar
  putMVar mvar $ do
    res <- (Right <$> action) `catchT` (return . Left)
    liftIO $ putMVar mvar' res
    return False
  res <- takeMVar mvar'
  case res of
    Left e -> print e >> fail (show e)
    Right r -> return r
