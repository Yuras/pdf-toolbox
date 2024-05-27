{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

module Prelude
(
  module P,

#if MIN_VERSION_base(4,11,0)
#else
  Semigroup(..),
#endif

#if MIN_VERSION_base(4,20,0)
#else
  foldl',
#endif

)
where

import "base" Prelude as P

#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup(Semigroup(..))
#endif

#if MIN_VERSION_base(4,20,0)
#else
import Data.List (foldl')
#endif
