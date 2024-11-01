{-# LANGUAGE Safe #-}
-- Removing this line should cause this module to not compile.
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-decls:recDef #-}

module Test.UnannName
  ( recDef,
    nonRecDef,
  )
where

import "base" Control.Category (id)

recDef :: a -> b
recDef = recDef

nonRecDef :: a -> a
nonRecDef = id
