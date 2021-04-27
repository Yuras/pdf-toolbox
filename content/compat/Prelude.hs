{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

module Prelude
(
  module P,

#if MIN_VERSION_base(4,11,0)
#else
  Semigroup(..),
#endif
)
where

import "base" Prelude as P

#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup(Semigroup(..))
#endif
