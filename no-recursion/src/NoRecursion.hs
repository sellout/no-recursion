{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}

-- | A plugin that identifies and reports on uses of recursion. The name evokes
--   a language pragma – implying a @Recursion@ pragma that is enabled by
--   default.
module NoRecursion (plugin) where

import safe "base" Control.Applicative (Applicative (pure))
import safe "base" Control.Category (Category ((.)))
import safe "base" Control.Exception (ErrorCall (ErrorCall), throwIO)
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bool (not, (&&), (||))
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Foldable (Foldable (foldMap, toList), all)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor (fmap), (<$>))
import safe "base" Data.List (filter, intercalate, isPrefixOf, null)
import safe "base" Data.List.NonEmpty (NonEmpty, nonEmpty)
import safe "base" Data.Maybe (maybe)
import safe "base" Data.Semigroup (Semigroup ((<>)))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (fst)
#if MIN_VERSION_ghc(9, 0, 0)
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

plugin :: Plugins.Plugin
plugin = defaultPurePlugin {Plugins.installCoreToDos = \_opts -> pure . install}

install :: [Plugins.CoreToDo] -> [Plugins.CoreToDo]
install = (Plugins.CoreDoPluginPass "add NoRecursion rule" noRecursionPass :)

noRecursionPass :: Plugins.ModGuts -> Plugins.CoreM Plugins.ModGuts
noRecursionPass guts = do
  dflags <- Plugins.getDynFlags
  either
    ( \recs ->
        Plugins.liftIO . throwIO . ErrorCall $
          "something recursive:\n"
            <> intercalate "\n" (toList $ formatRecursionRecord dflags <$> recs)
    )
    (\binds -> pure guts {Plugins.mg_binds = binds})
    . failOnRecursion dflags
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

failOnRecursion ::
  (Plugins.Outputable b) =>
  Plugins.DynFlags ->
  [Plugins.Bind b] ->
  Either (NonEmpty (RecursionRecord b)) [Plugins.Bind b]
failOnRecursion dflags original =
  maybe (pure original) Left
    . nonEmpty
    -- __TODO__: Default method implementations seem to cause mutual recursion
    --           with the instance, so here we filter them out, but this
    --           probably lets some real mutual recursion slip through.
    . filter
      ( \(RecursionRecord context recs) ->
          not $
            null context
              && all
                ( \var ->
                    let v = Plugins.showSDoc dflags $ Plugins.ppr var
                     in "$c" `isPrefixOf` v || "$f" `isPrefixOf` v
                )
                recs
      )
    $ recursiveCallsForBind =<< original

addBindingReference :: b -> [RecursionRecord b] -> [RecursionRecord b]
addBindingReference var =
  fmap (\(RecursionRecord context recs) -> RecursionRecord (var : context) recs)

recursiveCallsForBind :: Plugins.Bind b -> [RecursionRecord b]
recursiveCallsForBind = \case
  Plugins.NonRec v rhs -> addBindingReference v $ collectRecursiveCalls rhs
  Plugins.Rec binds ->
    let nestedRecursion =
          foldMap
            (\(v, rhs) -> addBindingReference v $ collectRecursiveCalls rhs)
            binds
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
