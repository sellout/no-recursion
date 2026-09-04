{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Finding recursion in Core. The traversal is generic in the binder and reads
-- recursion off `Core.Rec` binding groups, so it needs nothing from a compiler
-- session — deciding what to do about what it finds is somebody else’s job.
module GHC.Recursion
  ( Record (Record),
    context,
    inAlt,
    inBind,
    inExpr,
    occurrences,
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

-- | One place recursion was found.
--
-- @since 99999
type Record :: Type -> Type
data Record b = Record
  { -- | The enclosing binders, outermost last.
    --
    -- @since 99999
    context :: [b],
    -- | The binders of the recursive group itself.
    --
    -- @since 99999
    occurrences :: NonEmpty b
  }

addBindingReference :: b -> [Record b] -> [Record b]
addBindingReference var =
  fmap (\(Record context recs) -> Record (var : context) recs)

-- | Collects the recursion in a binding, and in everything under it.
--
-- @since 99999
inBind :: Core.Bind b -> [Record b]
inBind =
  let recInExpr v = addBindingReference v . inExpr
   in \case
        Core.NonRec v rhs -> recInExpr v rhs
        Core.Rec binds ->
          let nestedRecursion = foldMap (uncurry recInExpr) binds
           in maybe
                nestedRecursion
                (\bnds -> Record [] (fst <$> bnds) : nestedRecursion)
                $ nonEmpty binds

-- | This collects all identifiable recursion points in an expression.
--
-- @since 99999
inExpr :: Core.Expr b -> [Record b]
inExpr = \case
  Core.App f a -> inExpr f <> inExpr a
  Core.Case scrut _ _ alts ->
    inExpr scrut <> foldMap inAlt alts
  Core.Cast e _ -> inExpr e
  Core.Coercion _ -> []
  Core.Lam _ body -> inExpr body
  Core.Let bind e -> inBind bind <> inExpr e
  Core.Lit _ -> []
  Core.Tick _ body -> inExpr body
  Core.Type _ -> []
  Core.Var _ -> []

-- | Collects the recursion in one alternative of a @case@.
--
-- @since 99999
inAlt :: Core.Alt b -> [Record b]
inAlt (Core.Alt _ _ rhs) = inExpr rhs
