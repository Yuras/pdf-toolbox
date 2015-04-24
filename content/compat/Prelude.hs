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

import "base" Prelude as P

#if MIN_VERSION_base(4,8,0)
#else
import Data.Functor ((<$>))
import Data.Monoid(Monoid(..))
import Control.Applicative (Applicative(..))
#endif
