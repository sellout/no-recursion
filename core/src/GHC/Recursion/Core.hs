{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Finding recursion in Core. The traversal is generic in the binder, and the
-- two things it needs to know about a binder — what to call it and what
-- annotations it carries — arrive as functions, so nothing here needs a
-- compiler session.
module GHC.Recursion.Core (failOnRecursion) where

import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bool (Bool (True), not, (&&), (||))
import safe "base" Data.Either (Either (Left))
import safe "base" Data.Foldable
  ( all,
    any,
    elem,
    foldMap,
    notElem,
    traverse_,
  )
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.List (filter, isPrefixOf, null)
import safe "base" Data.List.NonEmpty (NonEmpty, nonEmpty)
import safe "base" Data.Maybe (maybe)
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (fst, uncurry)
import "ghc" GHC.Plugins qualified as Plugins
import safe "this" GHC.Recursion.Options
  ( Opts,
    allowRecursion,
    ignoreMethodCycles,
    ignoredDecls,
    ignoredMethods,
  )
import safe "this" GHC.Recursion.Record
  ( RecursionRecord (RecursionRecord),
  )

-- | The whole analysis.
--
--   `Left` carries every recursion that survived the options; `Right` means
--   there was none.
--
-- @since 99999
failOnRecursion ::
  -- | A function that returns the name of a binder
  (b -> String) ->
  -- | A function that returns the annotations on a binder
  (b -> [String]) ->
  -- | The annotations on the module
  [String] ->
  Opts ->
  [Plugins.Bind b] ->
  Either (NonEmpty (RecursionRecord b)) ()
failOnRecursion
  render
  annsOf
  modAnns
  opts
  original =
    traverse_ Left
      . nonEmpty
      -- __TODO__: Default method implementations seem to cause mutual
      --           recursion with the instance, so here we filter them out,
      --           but this probably lets some real mutual recursion slip
      --           through.
      . filter
        ( not
            . \(RecursionRecord context recs) ->
              ignoreMethodCycles opts && null context && all (isInternalName . render) recs
                || any (flip elem (ignoredDecls opts) . render) recs
                || any (flip elem (("$c" <>) <$> ignoredMethods opts) . render) context
        )
      $ recursiveCallsForBind
        =<< filter
          ( not
              . allowBind
                (moduleAllowsRecursion (allowRecursion opts) modAnns)
                annsOf
          )
          original

recursionAnnotation :: String
recursionAnnotation = "Recursion"

noRecursionAnnotation :: String
noRecursionAnnotation = "NoRecursion"

moduleAllowsRecursion :: Bool -> [String] -> Bool
moduleAllowsRecursion allowRecursion' modAnns =
  (allowRecursion' || elem recursionAnnotation modAnns)
    && notElem noRecursionAnnotation modAnns

-- | Whether a rendered binder name is one the desugarer invented for a class
--   method or a dictionary.
isInternalName :: String -> Bool
isInternalName v = "$c" `isPrefixOf` v || "$f" `isPrefixOf` v

addBindingReference :: b -> [RecursionRecord b] -> [RecursionRecord b]
addBindingReference var =
  fmap (\(RecursionRecord context recs) -> RecursionRecord (var : context) recs)

allowBind :: Bool -> (b -> [String]) -> Plugins.Bind b -> Bool
allowBind modAllowsRecursion annsOf = \case
  Plugins.NonRec {} -> True
  Plugins.Rec bs -> all (recursionAllowed modAllowsRecursion annsOf . fst) bs

recursionAllowed :: Bool -> (b -> [String]) -> b -> Bool
recursionAllowed modAllowsRecursion annsOf var =
  let strAnns = annsOf var
   in (modAllowsRecursion || elem recursionAnnotation strAnns)
        && notElem noRecursionAnnotation strAnns

recursiveCallsForBind :: Plugins.Bind b -> [RecursionRecord b]
recursiveCallsForBind =
  let collectCalls v = addBindingReference v . collectRecursiveCalls
   in \case
        Plugins.NonRec v rhs -> collectCalls v rhs
        Plugins.Rec binds ->
          let nestedRecursion = foldMap (uncurry collectCalls) binds
           in maybe
                nestedRecursion
                (\bnds -> RecursionRecord [] (fst <$> bnds) : nestedRecursion)
                $ nonEmpty binds

-- | This collects all identifiable recursion points in an expression.
collectRecursiveCalls :: Plugins.Expr b -> [RecursionRecord b]
collectRecursiveCalls = \case
  Plugins.App f a -> collectRecursiveCalls f <> collectRecursiveCalls a
  Plugins.Case scrut _ _ alts ->
    collectRecursiveCalls scrut <> foldMap recursiveCallsForAlt alts
  Plugins.Cast e _ -> collectRecursiveCalls e
  Plugins.Coercion _ -> []
  Plugins.Lam _ body -> collectRecursiveCalls body
  Plugins.Let bind e -> recursiveCallsForBind bind <> collectRecursiveCalls e
  Plugins.Lit _ -> []
  Plugins.Tick _ body -> collectRecursiveCalls body
  Plugins.Type _ -> []
  Plugins.Var _ -> []

recursiveCallsForAlt :: Plugins.Alt b -> [RecursionRecord b]
recursiveCallsForAlt (Plugins.Alt _ _ rhs) = collectRecursiveCalls rhs
