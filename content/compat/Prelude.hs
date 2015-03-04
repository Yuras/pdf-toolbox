{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

module Prelude
(
  module P
)
where

#if MIN_VERSION_base(4,6,0)
import "base" Prelude as P
#else
import "base" Prelude as P hiding (catch)
#endif
