{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Copyright: 2025 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- __TODO__: Make a separate package with general plugin utilities.
module PluginUtils
  ( defaultPurePlugin,
    Annotations,
    getAnnotations,
  )
where

import safe "base" Data.Bifunctor (first)
import safe "base" Data.Data (Data)
import safe "base" Data.Function (($))
import safe "base" Data.Functor ((<$>))
import safe "base" Data.Kind (Type)
import "ghc" GHC.Plugins qualified as Plugins

-- | The same as `Plugins.defaultPlugin`, but defaults to a pure plugin, rather
--   than an impure one.
--
-- @since 0.2.0
defaultPurePlugin :: Plugins.Plugin
defaultPurePlugin =
  Plugins.defaultPlugin {Plugins.pluginRecompile = Plugins.purePlugin}

-- | Annotations of type @a@ for a module – `fst` is the module-level
--   annotations and `Data.Tuple.snd` is a map of annotations for each name in
--   the module.
--
-- @since 0.2.0
type Annotations :: Type -> Type
type Annotations a = (a, Plugins.NameEnv a)

-- | Similar to `Plugins.getAnnotations`, but only returns the annotations for
--   the current module.
--
-- @since 0.2.0
getAnnotations :: (Data a) => Plugins.ModGuts -> Plugins.CoreM (Annotations [a])
getAnnotations guts =
  first
    ( \modAnns ->
        Plugins.lookupWithDefaultModuleEnv modAnns [] $
          Plugins.mg_module guts
    )
    <$> Plugins.getAnnotations Plugins.deserializeWithData guts
