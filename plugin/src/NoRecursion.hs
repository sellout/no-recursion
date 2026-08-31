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
import safe "base" Data.Bool (Bool (True))
import safe "base" Data.Either (either)
import safe "base" Data.Foldable (foldrM, toList)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.List (intercalate)
import safe "base" Data.Maybe (maybe)
import safe "base" Data.Semigroup ((<>))
import "ghc" GHC.Plugins qualified as Plugins
import "this" NoRecursion.Internal
  ( OptError (MissingValue, UnknownOption),
    Opts (allowRecursion, ignoreMethodCycles, ignoredDecls, ignoredMethods),
    defaultOpts,
    failOnRecursion,
    formatRecursionRecord,
    parseBoolOpt,
    parseListOpt,
    prettyOptError,
  )
import safe "this" PluginUtils
  ( defaultPurePlugin,
    getAnnotations,
    processOptions,
  )

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
