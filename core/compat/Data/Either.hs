{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

module Data.Either
(
  module Export,
#if MIN_VERSION_base(4,7,0)
#else
  isRight,
  isLeft,
#endif
)
where

import "base" Data.Either as Export

#if MIN_VERSION_base(4,7,0)
#else
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft = not . isRight
#endif
