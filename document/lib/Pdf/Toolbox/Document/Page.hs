{-# LANGUAGE OverloadedStrings #-}

-- | PDF document page

module Pdf.Toolbox.Document.Page
(
  Page,
  pageParentNode,
  pageContents,
  pageMediaBox,
  pageFontDicts
)
where

import Control.Monad

import Pdf.Toolbox.Core

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
