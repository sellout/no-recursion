{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Test.AnnModule
  ( recDef,
    nonRecDef,
  )
where

import safe "base" Control.Category (id)

-- Removing this line should cause this module to not compile.
{-# ANN module "Recursion" #-}

-- | A simple self-recursive definition.
--
-- @since 0.1.1
recDef :: a -> b
recDef = recDef

-- | A trivial non-recursive definition.
--
-- @since 0.1.1
nonRecDef :: a -> a
nonRecDef = id
{-# ANN nonRecDef "NoRecursion" #-}
