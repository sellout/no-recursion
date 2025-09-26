{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}

-- | A plugin that identifies and reports on uses of recursion. The name evokes
--   a language pragma – implying a @Recursion@ pragma that is enabled by
--   default.
module NoRecursion (plugin) where

import safe "base" Control.Applicative (liftA2, pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Exception (ErrorCall (ErrorCall), throwIO)
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bool (Bool (False, True), not, (&&), (||))
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Foldable
  ( all,
    any,
    elem,
    foldMap,
    foldr,
    foldrM,
    notElem,
    toList,
    traverse_,
  )
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.List (filter, intercalate, isPrefixOf, null)
import safe "base" Data.List.NonEmpty (NonEmpty, nonEmpty)
import safe "base" Data.Maybe (maybe)
import safe "base" Data.Semigroup (Semigroup ((<>)), (<>))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (curry, fst, uncurry)
import safe "this" PluginUtils
  ( Annotations,
    defaultPurePlugin,
    getAnnotations,
    processOptions,
  )
#if MIN_VERSION_ghc(9, 0, 0)
import qualified "ghc" GHC.Plugins as Plugins
#else
import qualified "ghc" GhcPlugins as Plugins
#endif

-- | The entrypoint for the "NoRecursion" plugin.
plugin :: Plugins.Plugin
plugin =
  defaultPurePlugin
    { Plugins.installCoreToDos = \opts -> liftA2 install (parseOpts opts) . pure
    }

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
defaultOpts :: Opts
defaultOpts =
  Opts
    { allowRecursion = False,
      ignoreMethodCycles = True,
      ignoredDecls = [],
      ignoredMethods = []
    }

data OptError
  = MissingValue String
  | UnknownOption String
  | UnknownValue String String

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

parseBoolOpt :: String -> Either OptError Bool
parseBoolOpt = \case
  "true" -> pure True
  "false" -> pure False
  value -> Left $ UnknownValue "Bool" value

parseListOpt :: String -> [String]
parseListOpt =
  foldr
    ( curry $ \case
        (',', elems) -> [] : elems
        (c, []) -> [[c]]
        (c, curr : elems) -> (c : curr) : elems
    )
    []

parseOpts :: [Plugins.CommandLineOption] -> Plugins.CoreM Opts
parseOpts =
  foldrM
    ( \(name, mvalue) opts ->
        case name of
          "allow-recursion" ->
            either (err opts) (\v -> pure opts {allowRecursion = v}) $
              maybe (pure True) parseBoolOpt mvalue
          "ignore-method-cycles" ->
            either (err opts) (\v -> pure opts {ignoreMethodCycles = v}) $
              maybe (pure True) parseBoolOpt mvalue
          "ignore-decls" ->
            maybe
              (err opts $ MissingValue name)
              ( \v ->
                  pure opts {ignoredDecls = parseListOpt v <> ignoredDecls opts}
              )
              mvalue
          "ignore-methods" ->
            maybe
              (err opts $ MissingValue name)
              ( \v ->
                  pure
                    opts
                      { ignoredMethods = parseListOpt v <> ignoredMethods opts
                      }
              )
              mvalue
          _ -> err opts $ UnknownOption name
    )
    defaultOpts
    . processOptions
  where
    err opts = fmap (\() -> opts) . Plugins.errorMsg . prettyOptError

install :: Opts -> [Plugins.CoreToDo] -> [Plugins.CoreToDo]
install opts =
  (Plugins.CoreDoPluginPass "add NoRecursion rule" (noRecursionPass opts) :)

noRecursionPass :: Opts -> Plugins.ModGuts -> Plugins.CoreM Plugins.ModGuts
noRecursionPass opts guts = do
  dflags <- Plugins.getDynFlags
  anns <- getAnnotations guts
  either
    ( \recs ->
        Plugins.liftIO . throwIO . ErrorCall $
          "encountered recursion, which has been disabled:\n"
            <> intercalate "\n" (toList $ formatRecursionRecord dflags <$> recs)
    )
    (\() -> pure guts)
    . failOnRecursion dflags opts anns
    $ Plugins.mg_binds guts

data RecursionRecord b = RecursionRecord [b] (NonEmpty b)

formatRecursionRecord ::
  (Plugins.Outputable b) => Plugins.DynFlags -> RecursionRecord b -> String
formatRecursionRecord dflags (RecursionRecord context recs) =
  maybe
    "at the top level"
    ( \v ->
        "in "
          <> intercalate
            " >> "
            (Plugins.showSDoc dflags . Plugins.ppr <$> toList v)
    )
    (nonEmpty context)
    <> ", the following bindings were recursive: "
    <> intercalate ", " (Plugins.showSDoc dflags . Plugins.ppr <$> toList recs)

recursionAnnotation :: String
recursionAnnotation = "Recursion"

noRecursionAnnotation :: String
noRecursionAnnotation = "NoRecursion"

moduleAllowsRecursion :: Bool -> [String] -> Bool
moduleAllowsRecursion allowRecursion modAnns =
  (allowRecursion || elem recursionAnnotation modAnns)
    && notElem noRecursionAnnotation modAnns

getName :: Plugins.DynFlags -> Plugins.CoreBndr -> String
getName dflags = Plugins.showSDoc dflags . Plugins.ppr

isInternalName :: Plugins.DynFlags -> Plugins.CoreBndr -> Bool
isInternalName dflags var =
  let v = getName dflags var
   in "$c" `isPrefixOf` v || "$f" `isPrefixOf` v

failOnRecursion ::
  Plugins.DynFlags ->
  Opts ->
  Annotations [String] ->
  [Plugins.CoreBind] ->
  Either (NonEmpty (RecursionRecord Plugins.CoreBndr)) ()
failOnRecursion
  dflags
  opts
  (modAnns, nameAnns)
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
              ignoreMethodCycles opts && null context && all (isInternalName dflags) recs
                || any (flip elem (ignoredDecls opts) . getName dflags) recs
                || any (flip elem (("$c" <>) <$> ignoredMethods opts) . getName dflags) context
        )
      $ recursiveCallsForBind
        =<< filter
          ( not
              . allowBind
                (moduleAllowsRecursion (allowRecursion opts) modAnns)
                nameAnns
          )
          original

addBindingReference :: b -> [RecursionRecord b] -> [RecursionRecord b]
addBindingReference var =
  fmap (\(RecursionRecord context recs) -> RecursionRecord (var : context) recs)

allowBind :: Bool -> Plugins.NameEnv [String] -> Plugins.CoreBind -> Bool
allowBind modAllowsRecursion anns = \case
  Plugins.NonRec {} -> True
  Plugins.Rec bs -> all (recursionAllowed modAllowsRecursion anns . fst) bs

recursionAllowed :: Bool -> Plugins.NameEnv [String] -> Plugins.Var -> Bool
recursionAllowed modAllowsRecursion anns var =
  let strAnns =
        Plugins.lookupWithDefaultUFM_Directly anns [] $ Plugins.getUnique var
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
#if MIN_VERSION_ghc(9, 2, 0)
recursiveCallsForAlt (Plugins.Alt _ _ rhs) = collectRecursiveCalls rhs
#else
recursiveCallsForAlt (_, _, rhs) = collectRecursiveCalls rhs
#endif
