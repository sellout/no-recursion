{-# LANGUAGE Unsafe #-}

module Test.AnnName
  ( recDef,
    nonRecDef,
  )
where

import safe "base" Control.Category (id)

recDef :: a -> b
recDef = recDef
{-# ANN recDef "Recursion" #-}

nonRecDef :: a -> a
nonRecDef = id
