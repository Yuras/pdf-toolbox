
module Main
(
  main
)
where

import Data.String
import Data.Functor
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import System.IO
import Graphics.UI.Gtk hiding (Rectangle)
import Graphics.Rendering.Cairo hiding (transform)

import Pdf.Toolbox.Document
import Pdf.Toolbox.Content.Ops
import Pdf.Toolbox.Content.Parser
import Pdf.Toolbox.Content.Processor
import Pdf.Toolbox.Content.Transform

main :: IO ()
main = do
  [file] <- initGUI
  mvar <- newEmptyMVar
  withBinaryFile file ReadMode $ \h -> do
  _ <- forkIO $ pdfThread h mvar

  (rootNode, totalPages) <- pdfSync mvar $ do
    pdf <- document
    enc <- documentEncryption pdf
    when (isJust enc) $
      liftIO $ print "WARNING: Document is encrypted, it is not fully supported yet"
    root <- documentCatalog pdf >>= catalogPageNode
    total <- pageNodeNKids root
    return (root, total)

  firstPage <- pdfSync mvar $ do
    pageNodePageByNum rootNode 0

  page <- newIORef (firstPage, 0)

  window <- windowNew
  set window [
    windowDefaultWidth := 300,
    windowDefaultHeight := 300
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
    contents <- pageContents page
    streams <- forM contents $ \ref -> do
      s@(Stream dict _) <- lookupObject ref >>= toStream
      len <- lookupDict (fromString "Length") dict >>= deref >>= fromObject >>= intValue
      return (s, len)
    ris <- getRIS
    is <- parseContentStream ris knownFilters streams

    let loop p = do
          next <- readNextOperator is
          case next of
            Nothing -> return ()
            Just op -> do
              case op of
                (Op_Tj, [OStr (Str str)]) -> do
                  let gstate = prState p
                      tm = gsTextMatrix gstate
                      pos = transform tm (Vector 0 0)
                  liftIO $ writeChan chan $ Just (pos, BS8.unpack str)
                (Op_TJ, [OArray (Array array)]) -> do
                  let gstate = prState p
                      tm = gsTextMatrix gstate
                      pos = transform tm (Vector 0 0)
                      toS (OStr (Str s)) = BS8.unpack s
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
