{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- A plugin that identifies and reports on uses of recursion. The name evokes a
-- language pragma – implying a @Recursion@ pragma that is enabled by default.
module NoRecursion (plugin) where

import safe "base" Control.Applicative (liftA2, pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Foldable (foldrM, toList)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.List (intercalate, reverse)
import safe "base" Data.Semigroup ((<>))
import "ghc" GHC.Plugins qualified as Plugins
import "this" NoRecursion.Internal
  ( Opts (allowRecursion, ignoreMethodCycles, ignoredDecls, ignoredMethods),
    defaultOpts,
    failOnRecursion,
    formatRecursionRecord,
  )
import safe "this" PluginUtils (defaultPurePlugin, getAnnotations)
import safe "this" PluginUtils.Options qualified as Opts

-- | The entrypoint for the "NoRecursion" plugin.
--
-- @since 0.1.0
plugin :: Plugins.Plugin
plugin =
  defaultPurePlugin
    { Plugins.installCoreToDos = \opts -> liftA2 install (parseOpts opts) . pure
    }

parseOpts :: [Plugins.CommandLineOption] -> Plugins.CoreM Opts
parseOpts =
  foldrM
    ( \opt opts ->
        let (name, mvalue) = Opts.process opt
            err =
              fmap (\() -> opts)
                . Plugins.errorMsg
                . Opts.prettyError "NoRecursion" name
         in either err pure case name of
              "allow-recursion" ->
                (\v -> opts {allowRecursion = v}) <$> Opts.parseBool mvalue
              "ignore-method-cycles" ->
                (\v -> opts {ignoreMethodCycles = v}) <$> Opts.parseBool mvalue
              "ignore-decls" ->
                (\v -> opts {ignoredDecls = v <> ignoredDecls opts})
                  <$> Opts.parseRequiringVal (pure . Opts.parseList) "List" mvalue
              "ignore-methods" ->
                (\v -> opts {ignoredMethods = v <> ignoredMethods opts})
                  <$> Opts.parseRequiringVal (pure . Opts.parseList) "List" mvalue
              _ -> Left Opts.UnknownOption
    )
    defaultOpts
    -- NOTE: Starting with GHC 8.6, plugin option order is reversed from what is
    --       given on the command line. This restores it, so that the last option
    --       wins.
    . reverse

install :: Opts -> [Plugins.CoreToDo] -> [Plugins.CoreToDo]
install opts =
  (Plugins.CoreDoPluginPass "add NoRecursion rule" (noRecursionPass opts) :)

noRecursionPass :: Opts -> Plugins.ModGuts -> Plugins.CoreM Plugins.ModGuts
noRecursionPass opts guts = do
  dflags <- Plugins.getDynFlags
  (modAnns, nameAnns) <- getAnnotations guts
  let render = Plugins.showSDoc dflags . Plugins.ppr
  either
    ( \recs ->
        Plugins.liftIO . Plugins.throwGhcExceptionIO . Plugins.ProgramError $
          "encountered recursion, which has been disabled:\n"
            <> intercalate "\n" (toList $ formatRecursionRecord render <$> recs)
    )
    (\() -> pure guts)
    . failOnRecursion
      render
      (Plugins.lookupWithDefaultUFM_Directly nameAnns [] . Plugins.getUnique)
      modAnns
      opts
    $ Plugins.mg_binds guts
