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
  ( OptError (MissingValue, UnknownOption, UnknownValue),
    Opts (Opts, allowRecursion, ignoreMethodCycles, ignoredDecls, ignoredMethods),
    defaultOpts,
    failOnRecursion,
    formatRecursionRecord,
    parseBoolOpt,
    parseListOpt,
    prettyOptError,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bool (Bool (False, True), not, (&&), (||))
import safe "base" Data.Either (Either (Left))
import safe "base" Data.Foldable
  ( all,
    any,
    elem,
    foldr,
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
import safe "base" Data.Tuple (curry, fst)
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

-- | The `Opts` we have if no @-fplugin-opts=NoRecursion:@ are provided.
--
-- - recursion is not allowed
-- - recursion cycles between methods is ignored (to avoid a breaking change)
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

-- | The ways an option can fail to be understood.
--
-- @since 99999
type OptError :: Type
data OptError
  = MissingValue String
  | UnknownOption String
  | UnknownValue String String

-- | Renders an `OptError` the way GHC renders its own option errors.
--
-- @since 99999
prettyOptError :: OptError -> Plugins.SDoc
prettyOptError =
  Plugins.text . \case
    MissingValue opt ->
      "plugin option ‘NoRecursion:" <> opt <> "’ is missing a value"
    UnknownOption name -> "unknown plugin option ‘NoRecursion:" <> name <> "’"
    UnknownValue typ value ->
      "an option for the NoRecursion plugin was expecting a "
        <> typ
        <> " but received ‘"
        <> value
        <> "’"

-- | Reads a `Bool`-valued option.
--
-- @since 99999
parseBoolOpt :: String -> Either OptError Bool
parseBoolOpt = \case
  "true" -> pure True
  "false" -> pure False
  value -> Left $ UnknownValue "Bool" value

-- | Reads a comma-separated option value as the list it denotes.
--
-- @since 99999
parseListOpt :: String -> [String]
parseListOpt =
  foldr
    ( curry $ \case
        (',', elems) -> [] : elems
        (c, []) -> [[c]]
        (c, curr : elems) -> (c : curr) : elems
    )
    []

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
