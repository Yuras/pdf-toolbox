
-- | 2d affine transform

module Pdf.Content.Transform
(
  Transform(..),
  Vector(..),
  identity,
  translation,
  scale,
  transform,
  translate,
  multiply
)
where

-- | Affine transform
data Transform a = Transform a a a a a a
  deriving Show

-- | 2d vector/point
data Vector a = Vector a a
  deriving Show

-- | Identity transform
identity :: Num a => Transform a
identity = Transform 1 0 0 1 0 0

-- | Translation
translation :: Num a => a -> a -> Transform a
translation tx ty = Transform 1 0 0 1 tx ty

-- | Scale
scale :: Num a => a -> a -> Transform a
scale sx sy = Transform sx 0 0 sy 0 0

-- | Apply transformation to vector
transform :: Num a => Transform a -> Vector a -> Vector a
transform (Transform a b c d e f) (Vector x y) =
  Vector (a * x + c * y + e) (b * x + d * y + f)

-- | Translate
translate :: Num a => a -> a -> Transform a -> Transform a
translate tx ty t = translation tx ty `multiply` t

-- | Combine two transformations
multiply :: Num a => Transform a -> Transform a -> Transform a
multiply (Transform a1 b1 c1 d1 e1 f1) (Transform a2 b2 c2 d2 e2 f2) =
  Transform a b c d e f
  where
  a = a1 * a2 + b1 * c2
  b = a1 * b2 + b1 * d2
  c = c1 * a2 + d1 * c2
  d = c1 * b2 + d1 * d2
  e = e1 * a2 + f1 * c2 + e2
  f = e1 * b2 + f1 * d2 + f2
