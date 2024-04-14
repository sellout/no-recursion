{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}

-- | A plugin that identifies and reports on uses of recursion. The name evokes
--   a language pragma – implying a @Recursion@ pragma that is enabled by
--   default.
module NoRecursion (plugin) where

import safe "base" Control.Applicative (Applicative (pure))
import safe "base" Control.Category (Category ((.)))
import safe "base" Control.Exception (ErrorCall (ErrorCall), throwIO)
import safe "base" Data.Either (either)
import safe "base" Data.Foldable (Foldable (toList))
import safe "base" Data.Function (($))
import safe "base" Data.Functor ((<$>))
import safe "base" Data.List (intercalate)
import safe "base" Data.List.NonEmpty (nonEmpty)
import safe "base" Data.Maybe (maybe)
import safe "base" Data.Semigroup (Semigroup ((<>)))
import safe "base" Data.String (String)
#if MIN_VERSION_ghc(9, 0, 0)
import qualified "ghc" GHC.Plugins as Plugins
#else
import qualified "ghc" GhcPlugins as Plugins
#endif
import "this" NoRecursion.Core
  ( RecursionRecord (RecursionRecord),
    failOnRecursion,
  )

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
