{-# LANGUAGE Safe #-}
-- Adding this line should cause this module to not compile.
-- {-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-method-cycles:false #-}
-- Removing this line should cause this module to not compile.
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-methods:sconcat,stimes #-}

-- | Without @-fplugin-opt=NoRecursion:no-ignore-default-impls@ specified during
--   compilation, default definitions won’t trigger an error.
module Test.IgnoreDefaultImpls
  ( Example (Empty, NotEmpty),
  )
where

import "base" Data.Function (($))
import "base" Data.Kind (Type)
import "base" Data.Monoid (Monoid, mappend, mempty)
import "base" Data.Semigroup (Semigroup, (<>))
import "base" Data.Tuple (curry)

type Example :: Type
data Example
  = Empty
  | NotEmpty

instance Semigroup Example where
  (<>) = curry $ \case
    (Empty, Empty) -> Empty
    (_, _) -> NotEmpty

instance Monoid Example where
  mappend = (<>)
  mempty = Empty
