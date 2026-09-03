{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- What the analysis reports, and how to render it. This module mentions no
-- compiler types at all — a binder is whatever the caller says it is.
module GHC.Recursion.Record
  ( RecursionRecord (..),
    formatRecursionRecord,
  )
where

import safe "base" Data.Foldable (toList)
import safe "base" Data.Functor ((<$>))
import safe "base" Data.Kind (Type)
import safe "base" Data.List (intercalate)
import safe "base" Data.List.NonEmpty (NonEmpty, nonEmpty)
import safe "base" Data.Maybe (maybe)
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)

-- | One place recursion was found: the enclosing binders, outermost last, and
--   the binders of the recursive group itself.
--
-- @since 99999
type RecursionRecord :: Type -> Type
data RecursionRecord b = RecursionRecord [b] (NonEmpty b)

-- | Renders a record for a human, using @render@ to name each binder.
--
-- @since 99999
formatRecursionRecord :: (b -> String) -> RecursionRecord b -> String
formatRecursionRecord render (RecursionRecord context recs) =
  maybe
    "at the top level"
    (\v -> "in " <> intercalate " >> " (render <$> toList v))
    (nonEmpty context)
    <> ", the following bindings were recursive: "
    <> intercalate ", " (render <$> toList recs)
