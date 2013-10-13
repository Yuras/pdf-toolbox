
-- | Process content stream operators maintaining graphics state
--
-- It is pretty experimental

module Pdf.Toolbox.Content.Processor
(
  Processor(..),
  GraphicsState(..),
  GlyphDecoder,
  Glyph(..),
  initialGraphicsState,
  mkProcessor,
  processOp
)
where

import Data.Monoid
import Data.Text (Text)
import Data.ByteString (ByteString)
import Control.Monad

import Pdf.Toolbox.Core

import Pdf.Toolbox.Content.Ops
import Pdf.Toolbox.Content.Transform

-- | Given font name and string, it should return list of glyphs
-- with bounding boxes defined in glyph space. It should not try
-- to position or scale glyphs in user space.
type GlyphDecoder = Name -> Str -> [Glyph]

-- | Glyph
data Glyph = Glyph {
  -- | The code as read from content stream
  glyphCode :: ByteString,
  -- | Top-left corner of glyph's bounding box
  glyphTopLeft :: Vector Double,
  -- | Bottom-right corner of glyph's bounding box
  glyphBottomRight :: Vector Double,
  -- | Text ectracted from the glyph
  glyphText :: Maybe Text
  }
  deriving Show

-- | Graphics state
data GraphicsState = GraphicsState {
  gsInText :: Bool,    -- ^ Indicates that we are inside text object
  gsCurrentTransformMatrix :: Transform Double,
  gsFont :: Maybe Name,
  gsFontSize :: Maybe Double,
  gsTextMatrix :: Transform Double,      -- ^ Defined only inside text object
  gsTextLineMatrix :: Transform Double,  -- ^ Defined only inside text object
  gsTextLeading :: Double
  }
  deriving Show

-- | Empty graphics state
initialGraphicsState :: GraphicsState
initialGraphicsState = GraphicsState {
  gsInText = False,
  gsCurrentTransformMatrix = identity,
  gsFont = Nothing,
  gsFontSize = Nothing,
  gsTextMatrix = identity,
  gsTextLineMatrix = identity,
  gsTextLeading = 0
  }

-- | Processor maintains graphics state
data Processor = Processor {
  prState :: GraphicsState,
  prStateStack :: [GraphicsState],
  prGlyphDecoder :: GlyphDecoder,
  prGlyphs :: [Glyph]
  }

-- | Create processor in initial state
mkProcessor :: Processor
mkProcessor = Processor {
  prState = initialGraphicsState,
  prStateStack = [],
  prGlyphDecoder = \_ _ -> [],
  prGlyphs = mempty
  }

-- | Process one operation
processOp :: Monad m => Operator -> Processor -> PdfE m Processor

processOp (Op_q, []) p = return p {prStateStack = prState p : prStateStack p}
processOp (Op_q, args) _ = left $ UnexpectedError $ "Op_q: wrong number of arguments: " ++ show args

processOp (Op_Q, []) p =
  case prStateStack p of
    [] -> left $ UnexpectedError "Op_Q: state is empty"
    (x:xs) -> return p {prState = x, prStateStack = xs}
processOp (Op_Q, args) _ = left $ UnexpectedError $ "Op_Q: wrong number of arguments: " ++ show args

processOp (Op_BT, []) p = do
  ensureInTextObject False p
  let gstate = prState p
  return p {prState = gstate {
    gsInText = True,
    gsTextMatrix = identity,
    gsTextLineMatrix = identity
    }}
processOp (Op_BT, args) _ = left $ UnexpectedError $ "Op_BT: wrong number of arguments: " ++ show args

processOp (Op_ET, []) p = do
  ensureInTextObject True p
  let gstate = prState p
  return p {prState = gstate {
    gsInText = False
    }}
processOp (Op_ET, args) _ = left $ UnexpectedError $ "Op_ET: wrong number of arguments: " ++ show args

processOp (Op_Td, [txo, tyo]) p = do
  ensureInTextObject True p
  tx <- fromObject txo >>= realValue
  ty <- fromObject tyo >>= realValue
  let gstate = prState p
      tm = translate tx ty $ gsTextLineMatrix gstate
  return p {prState = gstate {
    gsTextMatrix = tm,
    gsTextLineMatrix = tm
    }}
processOp (Op_Td, args) _ = left $ UnexpectedError $ "Op_Td: wrong number of arguments: " ++ show args

-- XXX: handle text leading here
processOp (Op_TD, [txo, tyo]) p = do
  l <- fromObject tyo >>= realValue
  p' <- processOp (Op_TL, [ONumber $ NumReal $ negate l]) p
  processOp (Op_Td, [txo, tyo]) p'
processOp (Op_TD, args) _ = left $ UnexpectedError $ "Op_TD: wrong number of arguments: " ++ show args

processOp (Op_Tm, [a', b', c', d', e', f']) p = do
  ensureInTextObject True p
  a <- fromObject a' >>= realValue
  b <- fromObject b' >>= realValue
  c <- fromObject c' >>= realValue
  d <- fromObject d' >>= realValue
  e <- fromObject e' >>= realValue
  f <- fromObject f' >>= realValue
  let gstate = prState p
      tm = Transform a b c d e f
  return p {prState = gstate {
    gsTextMatrix = tm,
    gsTextLineMatrix = tm
    }}
processOp (Op_Tm, args) _ = left $ UnexpectedError $ "Op_Tm: wrong number of arguments: " ++ show args

processOp (Op_T_star, []) p = do
  ensureInTextObject True p
  let gstate = prState p
      l = gsTextLeading gstate
  processOp (Op_TD, map (ONumber . NumReal) [0, negate l]) p
processOp (Op_T_star, args) _ = left $ UnexpectedError $ "Op_T_star: wrong number of arguments: " ++ show args

processOp (Op_TL, [lo]) p = do
  l <- fromObject lo >>= realValue
  let gstate = prState p
  return p {prState = gstate {
    gsTextLeading = l
    }}
processOp (Op_TL, args) _ = left $ UnexpectedError $ "Op_TL: wrong number of arguments: " ++ show args

processOp (Op_cm, [a', b', c', d', e', f']) p = do
  a <- fromObject a' >>= realValue
  b <- fromObject b' >>= realValue
  c <- fromObject c' >>= realValue
  d <- fromObject d' >>= realValue
  e <- fromObject e' >>= realValue
  f <- fromObject f' >>= realValue
  let gstate = prState p
      ctm = Transform a b c d e f `multiply` gsCurrentTransformMatrix gstate
  return p {prState = gstate {
    gsCurrentTransformMatrix = ctm
    }}
processOp (Op_cm, args) _ = left $ UnexpectedError $ "Op_cm: wrong number of arguments: " ++ show args

processOp (Op_Tf, [fontO, szO]) p = do
  font <- fromObject fontO
  sz <- fromObject szO >>= realValue
  let gstate = prState p
  return p {prState = gstate {
    gsFont = Just font,
    gsFontSize = Just sz
    }}
processOp (Op_Tf, args) _ = left $ UnexpectedError $ "Op_Tf: wrong number of agruments: " ++ show args

processOp (Op_Tj, [OStr str]) p = do
  let gstate = prState p
  fontName <-
    case gsFont gstate of
      Nothing -> left $ UnexpectedError "Op_Tj: font not set"
      Just fn -> return fn
  fontSize <-
    case gsFontSize gstate of
      Nothing -> left $ UnexpectedError "Op_Tj: font size not set"
      Just fs -> return fs
  let (tm, glyphs) = positionGlyghs fontSize (gsCurrentTransformMatrix gstate) (gsTextMatrix gstate) $ prGlyphDecoder p fontName str
  return p {
    prGlyphs = prGlyphs p ++ glyphs,
    prState = gstate {
      gsTextMatrix = tm
      }
    }
processOp (Op_Tj, args) _ = left $ UnexpectedError $ "Op_Tj: wrong number of agruments:" ++ show args

processOp (Op_TJ, [OArray (Array array)]) p = do
  let gstate = prState p
  fontName <-
    case gsFont gstate of
      Nothing -> left $ UnexpectedError "Op_Tj: font not set"
      Just fn -> return fn
  fontSize <-
    case gsFontSize gstate of
      Nothing -> left $ UnexpectedError "Op_Tj: font size not set"
      Just fs -> return fs
  let (textMatrix, glyphs) = loop (gsTextMatrix gstate) [] array
        where
        loop tm res [] = (tm, res)
        loop tm res (OStr str : rest) = let (tm', gs) = positionGlyghs fontSize (gsCurrentTransformMatrix gstate) tm (prGlyphDecoder p fontName str)
                                        in loop tm' (res ++ gs) rest
        loop tm res (ONumber (NumInt i): rest) = loop (translate (fromIntegral (-i) * fontSize / 1000) 0 tm) res rest
        loop tm res (ONumber (NumReal d): rest) = loop (translate (-d * fontSize / 1000) 0 tm) res rest
        loop tm res (_:rest) = loop tm res rest
  return p {
    prGlyphs = prGlyphs p ++ glyphs,
    prState = gstate {
      gsTextMatrix = textMatrix
      }
    }
processOp (Op_TJ, args) _ = left $ UnexpectedError $ "Op_TJ: wrong number of agruments:" ++ show args

processOp _ p = return p

ensureInTextObject :: Monad m => Bool -> Processor -> PdfE m ()
ensureInTextObject inText p =
  unless (inText == gsInText (prState p)) $ left $
    UnexpectedError $ "ensureInTextObject: expected: " ++ show inText ++ ", found: " ++ show (gsInText $ prState p)

positionGlyghs :: Double -> Transform Double -> Transform Double -> [Glyph] -> (Transform Double, [Glyph])
positionGlyghs fontSize ctm textMatrix = go textMatrix []
  where
  go tm res [] = (tm, reverse res)
  go tm res (g:gs) =
    let g' = g {
          glyphTopLeft = transform (multiply tm ctm) topLeft,
          glyphBottomRight = transform (multiply tm ctm) bottomRight
          }
        topLeft = transform (scale fontSize fontSize) $ glyphTopLeft g
        bottomRight = transform (scale fontSize fontSize) $ glyphBottomRight g
        width = case (topLeft, bottomRight) of
                  (Vector l _, Vector r _) -> r - l
        tm' = translate width 0 tm
    in go tm' (g':res) gs
