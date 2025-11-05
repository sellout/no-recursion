{-# LANGUAGE Unsafe #-}

module Test.AnnModule
  ( recDef,
    nonRecDef,
  )
where

import safe "base" Control.Category (id)

-- Removing this line should cause this module to not compile.
{-# ANN module "Recursion" #-}

recDef :: a -> b
recDef = recDef

nonRecDef :: a -> a
nonRecDef = id
{-# ANN nonRecDef "NoRecursion" #-}
