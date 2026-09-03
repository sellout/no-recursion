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
    processOptions,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Bifunctor (first, second)
import safe "base" Data.Data (Data)
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Kind (Type)
import safe "base" Data.List (drop, elemIndex, reverse, splitAt)
import safe "base" Data.Maybe (Maybe (Nothing), maybe)
import safe "base" Data.String (String)
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

-- | Starting with GHC 8.6, plugin option order is reversed from what’s provided
--   on the command line. This normalizes it to always match the command-line
--   order.
correctOptionOrder :: [Plugins.CommandLineOption] -> [Plugins.CommandLineOption]
correctOptionOrder = reverse

processOption :: Plugins.CommandLineOption -> (String, Maybe String)
processOption opt =
  maybe (opt, Nothing) (second (pure . drop 1) . flip splitAt opt) $
    elemIndex ':' opt

-- | This splits each option on `:`, returning a separate “value” if it exists.
--
-- @since 0.2.0
processOptions :: [Plugins.CommandLineOption] -> [(String, Maybe String)]
processOptions = fmap processOption . correctOptionOrder
