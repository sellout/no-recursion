{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
--
--  __TODO__: Make a separate package with general plugin utilities.
module PluginUtils
  ( defaultPurePlugin,
    Annotations,
    getAnnotations,
    processOptions,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Bifunctor (second)
import safe "base" Data.Data (Data)
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.List (drop, elemIndex, splitAt)
import safe "base" Data.Maybe (Maybe (Nothing), maybe)
import safe "base" Data.String (String)
#if MIN_VERSION_ghc(9, 0, 0)
import safe "base" Data.Bifunctor (first)
import qualified "ghc" GHC.Plugins as Plugins
#else
import qualified "ghc" GhcPlugins as Plugins
#endif
-- hlint wants these to be combined with the unconditional imports above.
{-# HLINT ignore "Use fewer imports" #-}
#if MIN_VERSION_ghc(8, 6, 0)
import safe "base" Data.List (reverse)
#else
import safe "base" Control.Category (id)
#endif

defaultPurePlugin :: Plugins.Plugin
#if MIN_VERSION_ghc(8, 6, 1)
defaultPurePlugin =
  Plugins.defaultPlugin {Plugins.pluginRecompile = Plugins.purePlugin}
#else
defaultPurePlugin = Plugins.defaultPlugin
#endif

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

-- | Starting with GHC 8.6, plugin option order is reversed from what’s provided
--   on the command line. This normalizes it to always match the command-line
--   order.
correctOptionOrder :: [Plugins.CommandLineOption] -> [Plugins.CommandLineOption]
#if MIN_VERSION_ghc(8, 6, 1)
correctOptionOrder = reverse
#else
correctOptionOrder = id
#endif

processOption :: Plugins.CommandLineOption -> (String, Maybe String)
processOption opt =
  maybe (opt, Nothing) (second (pure . drop 1) . flip splitAt opt) $
    elemIndex ':' opt

-- | This splits each option on `:`, returning a separate “value” if it exists.
processOptions :: [Plugins.CommandLineOption] -> [(String, Maybe String)]
processOptions = fmap processOption . correctOptionOrder
