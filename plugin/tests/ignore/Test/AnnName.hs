{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Test.AnnName
  ( recDef,
    nonRecDef,
  )
where

import safe "base" Control.Category (id)

-- | A simple self-recursive definition.
--
-- @since 0.1.1
recDef :: a -> b
recDef = recDef
-- Removing this line should cause this module to not compile.
{-# ANN recDef "Recursion" #-}

-- | A trivial non-recursive definition.
--
-- @since 0.1.1
nonRecDef :: a -> a
nonRecDef = id
