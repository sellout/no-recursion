{-# LANGUAGE Unsafe #-}

module Test.Name
  ( recDef,
    nonRecDef,
  )
where

import safe "base" Control.Category (Category (id))

recDef :: a -> b
recDef = recDef
{-# ANN recDef "Recursion" #-}

nonRecDef :: a -> a
nonRecDef = id
