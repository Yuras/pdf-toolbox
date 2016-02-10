{-# LANGUAGE OverloadedStrings #-}

-- | Content stream operators

module Pdf.Content.Ops
(
  Op(..),
  Expr(..),
  Operator,
  toOp
)
where

import Pdf.Core

import Data.ByteString (ByteString)

-- | Operator with arguments
type Operator = (Op, [Object])

-- | Content stream operators
data Op
  -- | Graphics State Operators
  = Op_q
  | Op_Q
  | Op_cm
  | Op_w
  | Op_J
  | Op_j
  | Op_M
  | Op_d
  | Op_ri
  | Op_i
  | Op_gs
  -- | Path Construction Operators
  | Op_m
  | Op_l
  | Op_c
  | Op_v
  | Op_y
  | Op_h
  | Op_re
  -- | Path Painting Operators
  | Op_S
  | Op_s
  | Op_f
  | Op_F
  | Op_f_star
  | Op_B
  | Op_B_star
  | Op_b
  | Op_b_star
  | Op_n
  -- | Clipping Path Operators
  | Op_W
  | Op_W_star
  -- | Text Object Operators
  | Op_BT
  | Op_ET
  -- | Text State Operators
  | Op_Tc
  | Op_Tw
  | Op_Tz
  | Op_TL
  | Op_Tf
  | Op_Tr
  | Op_Ts
  -- | Text Positioning Operators
  | Op_Td
  | Op_TD
  | Op_Tm
  | Op_T_star
  -- | Text Showing Operators
  | Op_Tj
  | Op_apostrophe
  | Op_quote
  | Op_TJ
  -- | Type 3 Font Operators
  | Op_d0
  | Op_d1
  -- | Color Operators
  | Op_CS
  | Op_cs
  | Op_SC
  | Op_SCN
  | Op_sc
  | Op_scn
  | Op_G
  | Op_g
  | Op_RG
  | Op_rg
  | Op_K
  | Op_k
  -- | Shading Operator
  | Op_sh
  -- | Inline Image Operators
  | Op_BI
  | Op_ID
  | Op_EI
  -- | XObject Operator
  | Op_Do
  -- | Marked Content Operators
  | Op_MP
  | Op_DP
  | Op_BMC
  | Op_BDC
  | Op_EMC
  -- | Compatibility Operators
  | Op_BX
  | Op_EX
  -- | Unknown
  | UnknownOp ByteString
  deriving (Show, Eq)

-- | Expression is a regular objects or an operators
data Expr
  = Obj Object
  | Op Op
  deriving (Show, Eq)

-- | Conversion to operator
toOp :: ByteString -> Op
toOp "q" = Op_q
toOp "Q" = Op_Q
toOp "cm" = Op_cm
toOp "w" = Op_w
toOp "J" = Op_J
toOp "j" = Op_j
toOp "M" = Op_M
toOp "d" = Op_d
toOp "ri" = Op_ri
toOp "i" = Op_i
toOp "gs" = Op_gs
toOp "m" = Op_m
toOp "l" = Op_l
toOp "c" = Op_c
toOp "v" = Op_v
toOp "y" = Op_y
toOp "h" = Op_h
toOp "re" = Op_re
toOp "S" = Op_S
toOp "s" = Op_s
toOp "f" = Op_f
toOp "F" = Op_F
toOp "f*" = Op_f_star
toOp "B" = Op_B
toOp "B*" = Op_B_star
toOp "b" = Op_b
toOp "b*" = Op_b_star
toOp "n" = Op_n
toOp "W" = Op_W
toOp "W*" = Op_W_star
toOp "BT" = Op_BT
toOp "ET" = Op_ET
toOp "Tc" = Op_Tc
toOp "Tw" = Op_Tw
toOp "Tz" = Op_Tz
toOp "TL" = Op_TL
toOp "Tf" = Op_Tf
toOp "Tr" = Op_Tr
toOp "Ts" = Op_Ts
toOp "Td" = Op_Td
toOp "TD" = Op_TD
toOp "Tm" = Op_Tm
toOp "T*" = Op_T_star
toOp "Tj" = Op_Tj
toOp "'" = Op_apostrophe
toOp "\"" = Op_quote
toOp "TJ" = Op_TJ
toOp "d0" = Op_d0
toOp "d1" = Op_d1
toOp "CS" = Op_CS
toOp "cs" = Op_cs
toOp "SC" = Op_SC
toOp "SCN" = Op_SCN
toOp "sc" = Op_sc
toOp "scn" = Op_scn
toOp "G" = Op_G
toOp "g" = Op_g
toOp "RG" = Op_RG
toOp "rg" = Op_rg
toOp "K" = Op_K
toOp "k" = Op_k
toOp "sh" = Op_sh
toOp "BI" = Op_BI
toOp "ID" = Op_ID
toOp "EI" = Op_EI
toOp "Do" = Op_Do
toOp "MP" = Op_MP
toOp "DP" = Op_DP
toOp "BMC" = Op_BMC
toOp "BDC" = Op_BDC
toOp "EMC" = Op_EMC
toOp "BX" = Op_BX
toOp "EX" = Op_EX
toOp str = UnknownOp str
