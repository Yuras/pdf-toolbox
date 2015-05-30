{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

module Prelude
(
  module P,

#if MIN_VERSION_base(4,8,0)
#else
  (<$>),
  Monoid(..),
  Applicative(..),
#endif
)
where

#if MIN_VERSION_base(4,6,0)
import "base" Prelude as P
#else
import "base" Prelude as P hiding (catch)
#endif

#if MIN_VERSION_base(4,8,0)
#else
import Data.Functor((<$>))
import Data.Monoid(Monoid(..))
import Control.Applicative(Applicative(..))
#endif
