
module Main
(
  main
)
where

import Data.String
import Data.Functor
import qualified Data.ByteString.Char8 as BS8
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

  page <- pdfSync mvar $ do
    pdf <- document
    enc <- documentEncryption pdf
    when (isJust enc) $
      liftIO $ print "WARNING: Document is encrypted, it is not fully supported yet"
    rootNode <- documentCatalog pdf >>= catalogPageNode
    pageNodePageByNum rootNode 0

  window <- windowNew
  set window [
    windowDefaultWidth := 300,
    windowDefaultHeight := 300
    ]
  _ <- on window deleteEvent $ liftIO mainQuit >> return True

  frame <- frameNew
  containerAdd window frame
  canvas <- drawingAreaNew
  containerAdd frame canvas

  widgetShowAll window
  draw <- widgetGetDrawWindow canvas
  _ <- on canvas exposeEvent $ do
    liftIO $ renderWithDrawable draw $ onDraw mvar page
    return True

  mainGUI

onDraw :: MVar (Pdf IO Bool) -> Page -> Render ()
onDraw mvar page = do
  setSourceRGB 1 1 1
  setLineWidth 1

  mediaBox@(Rectangle llx lly urx ury) <- liftIO $ pdfSync mvar $ pageMediaBox page
  liftIO $ print mediaBox

  chan <- liftIO $ startRender mvar page

  moveTo llx lly
  lineTo llx ury
  lineTo urx ury
  lineTo urx lly
  lineTo llx lly
  closePath
  fill

  setSourceRGB 0 0 0
  let loop = do
        circle <- liftIO $ readChan chan
        liftIO $ print circle
        case circle of
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
