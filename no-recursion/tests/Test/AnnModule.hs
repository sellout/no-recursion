{-# LANGUAGE Unsafe #-}

module Test.AnnModule
  ( recDef,
    nonRecDef,
  )
where

import safe "base" Control.Category (id)

{-# ANN module "Recursion" #-}

recDef :: a -> b
recDef = recDef

nonRecDef :: a -> a
nonRecDef = id
{-# ANN nonRecDef "NoRecursion" #-}
