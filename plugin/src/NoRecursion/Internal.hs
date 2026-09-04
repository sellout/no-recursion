{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- What the plugin does with what the traversal finds: the options that steer it,
-- the annotations that exempt a binding, and the report it produces.
--
-- This is not supported API. It is exposed so it can be tested without going
-- through a compiler session, and may change without a major bump.
module NoRecursion.Internal
  ( Opts (Opts, allowRecursion, ignoreMethodCycles, ignoredDecls, ignoredMethods),
    defaultOpts,
    failOnRecursion,
    formatRecursionRecord,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bool (Bool (False, True), not, (&&), (||))
import safe "base" Data.Either (Either (Left))
import safe "base" Data.Foldable
  ( all,
    any,
    elem,
    notElem,
    toList,
    traverse_,
  )
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Functor ((<$>))
import safe "base" Data.Kind (Type)
import safe "base" Data.List (filter, intercalate, isPrefixOf, null)
import safe "base" Data.List.NonEmpty (NonEmpty, nonEmpty)
import safe "base" Data.Maybe (maybe)
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (fst)
import safe "base" Text.Show (Show)
import "ghc" GHC.Plugins qualified as Plugins
import safe "recursion-analysis" GHC.Recursion (Record (Record), inBind)

-- | How the plugin should behave on the module being compiled.
--
-- @since 99999
type Opts :: Type
data Opts = Opts
  { allowRecursion :: Bool,
    ignoreMethodCycles :: Bool,
    ignoredDecls :: [String],
    ignoredMethods :: [String]
  }
  deriving stock (Show)

-- | The `Opts` we have if no @-fplugin-opt NoRecursion:@ are provided.
--
-- >>> defaultOpts
-- Opts {allowRecursion = False, ignoreMethodCycles = True, ignoredDecls = [], ignoredMethods = []}
--
--   `ignoredDecls` and `ignoredMethods` accumulate if they are given more than
--   once.
--
--   __NOTE__: `ignoreMethodCycles` will default to `False` in a future major
--             release. If you want it to remain `True`, set it explicitly.
--
-- @since 99999
defaultOpts :: Opts
defaultOpts =
  Opts
    { allowRecursion = False,
      ignoreMethodCycles = True,
      ignoredDecls = [],
      ignoredMethods = []
    }

-- | Renders a record for a human, using @render@ to name each binder.
--
-- @since 99999
formatRecursionRecord :: (b -> String) -> Record b -> String
formatRecursionRecord render (Record context recs) =
  maybe
    "at the top level"
    (\v -> "in " <> intercalate " >> " (render <$> toList v))
    (nonEmpty context)
    <> ", the following bindings were recursive: "
    <> intercalate ", " (render <$> toList recs)

recursionAnnotation :: String
recursionAnnotation = "Recursion"

noRecursionAnnotation :: String
noRecursionAnnotation = "NoRecursion"

moduleAllowsRecursion :: Bool -> [String] -> Bool
moduleAllowsRecursion allowRecursion modAnns =
  (allowRecursion || elem recursionAnnotation modAnns)
    && notElem noRecursionAnnotation modAnns

-- | Whether a rendered binder name is one the desugarer invented for a class
--   method or a dictionary.
isInternalName :: String -> Bool
isInternalName v = "$c" `isPrefixOf` v || "$f" `isPrefixOf` v

-- | The whole analysis, over any binder type.
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
  Either (NonEmpty (Record b)) ()
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
            . \(Record context recs) ->
              ignoreMethodCycles opts && null context && all (isInternalName . render) recs
                || any (flip elem (ignoredDecls opts) . render) recs
                || any (flip elem (("$c" <>) <$> ignoredMethods opts) . render) context
        )
      $ inBind
        =<< filter
          ( not
              . allowBind
                (moduleAllowsRecursion (allowRecursion opts) modAnns)
                annsOf
          )
          original

allowBind :: Bool -> (b -> [String]) -> Plugins.Bind b -> Bool
allowBind modAllowsRecursion annsOf = \case
  Plugins.NonRec {} -> True
  Plugins.Rec bs -> all (recursionAllowed modAllowsRecursion annsOf . fst) bs

recursionAllowed :: Bool -> (b -> [String]) -> b -> Bool
recursionAllowed modAllowsRecursion annsOf var =
  let strAnns = annsOf var
   in (modAllowsRecursion || elem recursionAnnotation strAnns)
        && notElem noRecursionAnnotation strAnns
