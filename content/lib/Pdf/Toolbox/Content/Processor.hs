
-- | Process content stream operators maintaining graphics state
--
-- It is pretty experimental

module Pdf.Toolbox.Content.Processor
(
  Processor(..),
  GraphicsState(..),
  processOp
)
where

import Pdf.Toolbox.Core

import Pdf.Toolbox.Content.Ops

-- | Graphics state
data GraphicsState = GraphicsState {
  }
  deriving Show

-- | Processor maintains graphics state
data Processor = Processor {
  prState :: GraphicsState,
  prStateStack :: [GraphicsState]
  }
  deriving Show

-- | Process one operation
processOp :: Monad m => Operator -> Processor -> PdfE m Processor

processOp (Op_q, []) p = return p {prStateStack = prState p : prStateStack p}
processOp (Op_q, args) _ = left $ UnexpectedError $ "Op_q: wrong number of arguments: " ++ show args

processOp (Op_Q, []) p =
  case prStateStack p of
    [] -> left $ UnexpectedError "Op_Q: state is empty"
    (x:xs) -> return p {prState = x, prStateStack = xs}
processOp (Op_Q, args) _ = left $ UnexpectedError $ "Op_Q: wrong number of arguments: " ++ show args

processOp _ p = return p
