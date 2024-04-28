{-# LANGUAGE Unsafe #-}

module Test.Name
  ( recDef,
    nonRecDef,
  )
where

-- NB: These unqualified modules come from semigroups in GHC <8, and base
--     otherwise.
import safe Data.Semigroup (Semigroup ((<>)))
import safe "base" Control.Category (Category (id))

recDef :: a -> b
recDef = recDef
{-# ANN recDef "Recursion" #-}

nonRecDef :: a -> a
nonRecDef = id

data Dummy = Dummy

-- | This should result in warnings, but not failures.
instance Semigroup Dummy where
  Dummy <> Dummy = Dummy
