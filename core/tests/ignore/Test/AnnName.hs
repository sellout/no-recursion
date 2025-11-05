{-# LANGUAGE Unsafe #-}

module Test.AnnName
  ( recDef,
    nonRecDef,
  )
where

import safe "base" Control.Category (id)

recDef :: a -> b
recDef = recDef
-- Removing this line should cause this module to not compile.
{-# ANN recDef "Recursion" #-}

nonRecDef :: a -> a
nonRecDef = id
