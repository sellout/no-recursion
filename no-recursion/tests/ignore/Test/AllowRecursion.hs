{-# LANGUAGE Safe #-}
-- Removing this line should cause this module to not compile.
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:allow-recursion:true #-}

module Test.AllowRecursion
  ( recDef,
    nonRecDef,
  )
where

import "base" Control.Category (id)

recDef :: a -> b
recDef = recDef

nonRecDef :: a -> a
nonRecDef = id
