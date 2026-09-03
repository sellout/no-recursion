{-# LANGUAGE Safe #-}
-- Removing this line should cause this module to not compile.
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:allow-recursion:true #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Test.AllowRecursion
  ( recDef,
    nonRecDef,
  )
where

import "base" Control.Category (id)

-- | A simple self-recursive definition.
--
-- @since 0.2.0
recDef :: a -> b
recDef = recDef

-- | A trivial non-recursive definition.
--
-- @since 0.2.0
nonRecDef :: a -> a
nonRecDef = id
