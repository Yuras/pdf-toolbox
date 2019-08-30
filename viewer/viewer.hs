{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Pdf.Content
import Pdf.Document

import qualified Data.Text as Text
import Data.IORef
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import System.Random (randomIO)
import System.Process
import System.Exit
import Graphics.UI.Gtk hiding (Rectangle, rectangle, FontMap)
import Graphics.Rendering.Cairo hiding (transform, Glyph)

data ViewerState = ViewerState {
  viewerPage :: Page,
  viewerPageNum :: Int,
  viewerRenderIM :: Bool,
  viewerRenderText :: Bool,
  viewerRenderGlyphs :: Bool
  }

main :: IO ()
main = do
  [file] <- initGUI

  withPdfFile file $ \pdf -> do
    (rootNode, totalPages, title) <- do
      encrypted <- isEncrypted pdf
      when encrypted $ do
        ok <- setUserPassword pdf ""
        unless ok $
          error "Password is wrong"

      doc <- document pdf

      title <- do
        maybe_info <- documentInfo doc
        case maybe_info of
          Nothing -> return Nothing
          Just info -> infoTitle info

      root <- documentCatalog doc >>= catalogPageNode
      total <- pageNodeNKids root

      return (root, total, title)

    firstPage <- pageNodePageByNum rootNode 0

    viewerState <- newIORef ViewerState {
      viewerPage = firstPage,
      viewerPageNum = 0,
      viewerRenderIM = False,
      viewerRenderText = True,
      viewerRenderGlyphs = False
      }

    let winTitle = fromMaybe "Untitled" title

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

    prevButton <- buttonNewWithLabel ("Prev" :: String)
    boxPackStart hbuttonBox prevButton PackNatural 0

    nextButton <- buttonNewWithLabel ("Next" :: String)
    boxPackStart hbuttonBox nextButton PackNatural 0

    renderPdfToggle <- checkButtonNewWithLabel ("Render via ImageMagick" :: String)
    set renderPdfToggle [
      toggleButtonActive := False
      ]
    boxPackStart hbuttonBox renderPdfToggle PackNatural 0

    renderTextToggle <- checkButtonNewWithLabel ("Render extracted text" :: String)
    set renderTextToggle [
      toggleButtonActive := True
      ]
    boxPackStart hbuttonBox renderTextToggle PackNatural 0

    renderGlyphsToggle <- checkButtonNewWithLabel ("Render glyphs" :: String)
    set renderGlyphsToggle [
      toggleButtonActive := False
      ]
    boxPackStart hbuttonBox renderGlyphsToggle PackNatural 0

    frame <- frameNew
    boxPackStart vbox frame PackGrow 0

    canvas <- drawingAreaNew
    containerAdd frame canvas

    _ <- on renderPdfToggle toggled $ do
      st <- get renderPdfToggle toggleButtonActive
      modifyIORef viewerState $ \s -> s {
        viewerRenderIM = st
        }
      widgetQueueDraw canvas

    _ <- on renderTextToggle toggled $ do
      st <- get renderTextToggle toggleButtonActive
      modifyIORef viewerState $ \s -> s {
        viewerRenderText = st
        }
      widgetQueueDraw canvas

    _ <- on renderGlyphsToggle toggled $ do
      st <- get renderGlyphsToggle toggleButtonActive
      modifyIORef viewerState $ \s -> s {
        viewerRenderGlyphs = st
        }
      widgetQueueDraw canvas

    _ <- on prevButton buttonActivated $ do
      num <- viewerPageNum <$> readIORef viewerState
      when (num > 0) $ do
        p <- pageNodePageByNum rootNode (num - 1)
        modifyIORef viewerState $ \s -> s {
          viewerPage = p,
          viewerPageNum = num - 1
          }
        widgetQueueDraw canvas

    _ <- on nextButton buttonActivated $ do
      num <- viewerPageNum <$> readIORef viewerState
      when (num < totalPages - 1) $ do
        p <- pageNodePageByNum rootNode (num + 1)
        modifyIORef viewerState $ \s -> s {
          viewerPage = p,
          viewerPageNum = num + 1
          }
        widgetQueueDraw canvas

    widgetShowAll window
    draw <- widgetGetDrawWindow canvas
    _ <- on canvas exposeEvent $ do
      liftIO $ renderWithDrawable draw $ onDraw file viewerState
      return True

    mainGUI

onDraw :: FilePath -> IORef ViewerState -> Render ()
onDraw file viewerState = do
  st <- liftIO $ readIORef viewerState
  let pg = viewerPage st
      num = viewerPageNum st

  when (viewerRenderIM st) $ do
    randomNum <- liftIO $ randomIO :: Render Int
    tmpDir <- liftIO $ getTemporaryDirectory
    let tmpFile = tmpDir </> ("pdf-toolbox-viewer-" ++ show randomNum ++ ".png")
    (_, _, _, procHandle) <- liftIO $ createProcess $ proc "convert" [file ++ "[" ++ show num ++ "]", tmpFile]
    hasPng <- (== ExitSuccess) <$> liftIO (waitForProcess procHandle)
    if hasPng
      then do
        surface <- liftIO $ imageSurfaceCreateFromPNG tmpFile
        setSourceSurface surface 0 0
        paint
        surfaceFinish surface
        liftIO $ removeFile tmpFile
      else liftIO $ putStrLn "Can't render pdf via ImageMagick. Please check that you have \"convert\" in PATH"

  setSourceRGB 1 1 1
  setLineWidth 1

  Rectangle llx lly urx ury <- liftIO $ pageMediaBox pg

  moveTo llx lly
  lineTo llx ury
  lineTo urx ury
  lineTo urx lly
  lineTo llx lly
  closePath
  stroke

  glyphs <- liftIO $ (concat . map spGlyphs) <$> pageExtractGlyphs pg
  forM_ glyphs $ \glyph -> do
    let Vector x1 y1 = glyphTopLeft glyph
        Vector x2 y2 = glyphBottomRight glyph
    when (viewerRenderText st) $ do
      setSourceRGB 0 0 0
      case glyphText glyph of
        Nothing -> return ()
        Just txt -> do
          moveTo x1 (ury - y1)
          showText $ Text.unpack txt
          stroke
    when (viewerRenderGlyphs st) $ do
      setSourceRGBA 0 0 0 0.2
      rectangle x1 (ury - y1) (x2 - x1) (y2 - y1)
      fill
