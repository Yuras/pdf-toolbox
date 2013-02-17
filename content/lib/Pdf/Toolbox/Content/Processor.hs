
-- | Process content stream operators maintaining graphics state
--
-- It is pretty experimental

module Pdf.Toolbox.Content.Processor
(
  Processor(..),
  GraphicsState(..),
  initialGraphicsState,
  mkProcessor,
  processOp
)
where

import Control.Monad

import Pdf.Toolbox.Core

import Pdf.Toolbox.Content.Ops
import Pdf.Toolbox.Content.Transform

-- | Graphics state
data GraphicsState = GraphicsState {
  gsInText :: Bool,    -- ^ Indicates that we are inside text object
  gsTextMatrix :: Transform Double,      -- ^ Defined only inside text object
  gsTextLineMatrix :: Transform Double,  -- ^ Defined only inside text object
  gsTextLeading :: Double
  }
  deriving Show

-- | Empty graphics state
initialGraphicsState :: GraphicsState
initialGraphicsState = GraphicsState {
  gsInText = False,
  gsTextMatrix = identity,
  gsTextLineMatrix = identity,
  gsTextLeading = 0
  }

-- | Processor maintains graphics state
data Processor = Processor {
  prState :: GraphicsState,
  prStateStack :: [GraphicsState]
  }
  deriving Show

-- | Create processor in initial state
mkProcessor :: Processor
mkProcessor = Processor {
  prState = initialGraphicsState,
  prStateStack = []
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

processOp _ p = return p

ensureInTextObject :: Monad m => Bool -> Processor -> PdfE m ()
ensureInTextObject inText p =
  unless (inText == gsInText (prState p)) $ left $
    UnexpectedError $ "ensureInTextObject: expected: " ++ show inText ++ ", found: " ++ show (gsInText $ prState p)
