{-# LANGUAGE Unsafe #-}

module Test.Module
  ( recDef,
    nonRecDef,
  )
where

import safe "base" Control.Category (Category (id))

{-# ANN module "Recursion" #-}

recDef :: a -> b
recDef = recDef

nonRecDef :: a -> a
nonRecDef = id
{-# ANN nonRecDef "NoRecursion" #-}
