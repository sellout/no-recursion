{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:allow-recursion #-}

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
