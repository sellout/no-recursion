{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Finding recursion in Core. The traversal is generic in the binder and reads
-- recursion off `Core.Rec` binding groups, so it needs nothing from a compiler
-- session — deciding what to do about what it finds is somebody else’s job.
module GHC.Recursion
  ( RecursionRecord (RecursionRecord),
    collectRecursiveCalls,
    recursiveCallsForAlt,
    recursiveCallsForBind,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Data.Foldable (foldMap)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Kind (Type)
import safe "base" Data.List.NonEmpty (NonEmpty, nonEmpty)
import safe "base" Data.Maybe (maybe)
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.Tuple (fst, uncurry)
import "ghc" GHC.Core qualified as Core

-- | One place recursion was found: the enclosing binders, outermost last, and
--   the binders of the recursive group itself.
--
-- @since 99999
type RecursionRecord :: Type -> Type
data RecursionRecord b = RecursionRecord [b] (NonEmpty b)

addBindingReference :: b -> [RecursionRecord b] -> [RecursionRecord b]
addBindingReference var =
  fmap (\(RecursionRecord context recs) -> RecursionRecord (var : context) recs)

-- | Collects the recursion in a binding, and in everything under it.
--
-- @since 99999
recursiveCallsForBind :: Core.Bind b -> [RecursionRecord b]
recursiveCallsForBind =
  let collectCalls v = addBindingReference v . collectRecursiveCalls
   in \case
        Core.NonRec v rhs -> collectCalls v rhs
        Core.Rec binds ->
          let nestedRecursion = foldMap (uncurry collectCalls) binds
           in maybe
                nestedRecursion
                (\bnds -> RecursionRecord [] (fst <$> bnds) : nestedRecursion)
                $ nonEmpty binds

-- | This collects all identifiable recursion points in an expression.
--
-- @since 99999
collectRecursiveCalls :: Core.Expr b -> [RecursionRecord b]
collectRecursiveCalls = \case
  Core.App f a -> collectRecursiveCalls f <> collectRecursiveCalls a
  Core.Case scrut _ _ alts ->
    collectRecursiveCalls scrut <> foldMap recursiveCallsForAlt alts
  Core.Cast e _ -> collectRecursiveCalls e
  Core.Coercion _ -> []
  Core.Lam _ body -> collectRecursiveCalls body
  Core.Let bind e -> recursiveCallsForBind bind <> collectRecursiveCalls e
  Core.Lit _ -> []
  Core.Tick _ body -> collectRecursiveCalls body
  Core.Type _ -> []
  Core.Var _ -> []

-- | Collects the recursion in one alternative of a @case@.
--
-- @since 99999
recursiveCallsForAlt :: Core.Alt b -> [RecursionRecord b]
recursiveCallsForAlt (Core.Alt _ _ rhs) = collectRecursiveCalls rhs
