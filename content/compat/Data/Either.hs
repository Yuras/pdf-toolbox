{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

module Data.Either
(
  module Export,
#if MIN_VERSION_base(4,7,0)
#else
  isRight,
#endif
)
where

import "base" Data.Either as Export

#if MIN_VERSION_base(4,7,0)
#else
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
#endif
