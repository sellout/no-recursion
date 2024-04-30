{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}

-- | A plugin that identifies and reports on uses of recursion. The name evokes
--   a language pragma – implying a @Recursion@ pragma that is enabled by
--   default.
module NoRecursion (plugin) where

-- NB: These unqualified modules come from semigroups in GHC <8, and base
--     otherwise.
import safe Data.List.NonEmpty (NonEmpty, nonEmpty)
import safe Data.Semigroup (Semigroup ((<>)))
import safe "base" Control.Applicative (Applicative (pure), liftA2)
import safe "base" Control.Category (Category ((.)))
import safe "base" Control.Exception (ErrorCall (ErrorCall), throwIO)
import safe "base" Control.Monad ((<=<))
import safe "base" Data.Bifunctor (Bifunctor (bimap))
import safe "base" Data.Bool (Bool (True), not, (&&), (||))
import safe "base" Data.Data (Data)
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Foldable
  ( Foldable (foldMap, toList),
    all,
    any,
    elem,
    notElem,
    traverse_,
  )
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor (fmap), (<$>))
import safe "base" Data.List (filter, intercalate, isPrefixOf, null, partition)
import safe "base" Data.Maybe (maybe)
import safe "base" Data.String (String)
import safe "base" Data.Tuple (fst, uncurry)
import safe "base" System.IO (hPutStrLn, stderr)
#if MIN_VERSION_ghc(9, 0, 0)
import safe "base" Data.Bifunctor (Bifunctor (first))
import qualified "ghc" GHC.Plugins as Plugins
#else
import qualified "ghc" GhcPlugins as Plugins
#endif

defaultPurePlugin :: Plugins.Plugin
#if MIN_VERSION_ghc(8, 6, 1)
defaultPurePlugin =
  Plugins.defaultPlugin {Plugins.pluginRecompile = Plugins.purePlugin}
#else
defaultPurePlugin = Plugins.defaultPlugin
#endif

-- | The entrypoint for the "NoRecursion" plugin.
plugin :: Plugins.Plugin
plugin = defaultPurePlugin {Plugins.installCoreToDos = \_opts -> pure . install}

install :: [Plugins.CoreToDo] -> [Plugins.CoreToDo]
install = (Plugins.CoreDoPluginPass "add NoRecursion rule" noRecursionPass :)

-- | Annotations of type @a@ for a module – `fst` is the module-level
--   annotations and `Data.Tuple.snd` is a map of annotations for each name in
--   the module.
type Annotations a = (a, Plugins.NameEnv a)

getAnnotations :: (Data a) => Plugins.ModGuts -> Plugins.CoreM (Annotations [a])
#if MIN_VERSION_ghc(9, 0, 1)
getAnnotations guts =
  first
    ( \modAnns ->
        Plugins.lookupWithDefaultModuleEnv modAnns [] $
          Plugins.mg_module guts
    )
    <$> Plugins.getAnnotations Plugins.deserializeWithData guts
#else
getAnnotations guts =
  ( \anns ->
      ( Plugins.lookupWithDefaultUFM
          anns
          []
          ( Plugins.ModuleTarget $ Plugins.mg_module guts ::
              Plugins.CoreAnnTarget
          ),
        anns
      )
  )
    <$> Plugins.getAnnotations Plugins.deserializeWithData guts
#endif

noRecursionPass :: Plugins.ModGuts -> Plugins.CoreM Plugins.ModGuts
noRecursionPass guts = do
  dflags <- Plugins.getDynFlags
  anns <- getAnnotations guts
  let formatRecords header =
        ((header <> ": recursion\n  • ") <>)
          . intercalate "\n  • "
          . toList
          . fmap (formatRecursionRecord dflags)
  fmap (\() -> guts)
    . Plugins.liftIO
    . uncurry (liftA2 (<>))
    . bimap
      (maybe (pure ()) (hPutStrLn stderr . formatRecords "Warning") . nonEmpty)
      (either (throwIO . ErrorCall . formatRecords "Error") pure)
    . failOnRecursion dflags anns
    $ Plugins.mg_binds guts

data RecursionRecord b = RecursionRecord [b] (NonEmpty b)

formatRecursionRecord ::
  (Plugins.Outputable b) => Plugins.DynFlags -> RecursionRecord b -> String
formatRecursionRecord dflags (RecursionRecord context recs) =
  maybe
    "at the top level"
    ( ("in " <>)
        . intercalate " » "
        . toList
        . fmap (Plugins.showSDoc dflags . Plugins.ppr)
    )
    (nonEmpty context)
    <> ", the following bindings were recursive: "
    <> intercalate ", " (Plugins.showSDoc dflags . Plugins.ppr <$> toList recs)

recursionAnnotation :: String
recursionAnnotation = "Recursion"

noRecursionAnnotation :: String
noRecursionAnnotation = "NoRecursion"

-- | Collects no-fatal and fatal occurrences of recursion. Currently, the only
--   non-fatal ones are from instance methods. This is because they can’t yet be
--   silenced with an annotation, so the only way around it is to explicitly
--   define them, which isn’t always desirable.
failOnRecursion ::
  Plugins.DynFlags ->
  Annotations [String] ->
  [Plugins.CoreBind] ->
  ( [RecursionRecord Plugins.CoreBndr],
    Either (NonEmpty (RecursionRecord Plugins.CoreBndr)) ()
  )
failOnRecursion dflags (modAnns, nameAnns) =
  let moduleAllowsRecursion =
        elem recursionAnnotation modAnns
          && notElem noRecursionAnnotation modAnns
   in fmap (traverse_ Left . nonEmpty)
        . partition
          ( \(RecursionRecord context recs) ->
              any
                (("$c" `isPrefixOf`) . Plugins.showSDoc dflags . Plugins.ppr)
                context
                || null context
                  && all
                    ( \var ->
                        -- __FIXME__: Is there a way to figure out the context
                        -- (@$c@, @$f@) of a variable without needing to
                        --  serialize it?
                        let v = Plugins.showSDoc dflags $ Plugins.ppr var
                         in "$c" `isPrefixOf` v || "$f" `isPrefixOf` v
                    )
                    recs
          )
        . ( recursiveCallsForBind
              <=< filter (not . allowBind moduleAllowsRecursion nameAnns)
          )

addBindingReference :: b -> [RecursionRecord b] -> [RecursionRecord b]
addBindingReference var =
  fmap (\(RecursionRecord context recs) -> RecursionRecord (var : context) recs)

allowBind :: Bool -> Plugins.NameEnv [String] -> Plugins.CoreBind -> Bool
allowBind moduleAllowsRecursion anns = \case
  Plugins.NonRec {} -> True
  Plugins.Rec bs -> all (recursionAllowed moduleAllowsRecursion anns . fst) bs

-- | Determine if recursion is allowed for a particular binding.
recursionAllowed :: Bool -> Plugins.NameEnv [String] -> Plugins.Var -> Bool
recursionAllowed moduleAllowsRecursion anns var =
  let strAnns =
        Plugins.lookupWithDefaultUFM_Directly anns [] $ Plugins.getUnique var
   in (moduleAllowsRecursion || elem recursionAnnotation strAnns)
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
