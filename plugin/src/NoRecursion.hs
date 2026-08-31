{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- A plugin that identifies and reports on uses of recursion. The name evokes a
-- language pragma – implying a @Recursion@ pragma that is enabled by default.
module NoRecursion (Opts, defaultOpts, plugin) where

import safe "base" Control.Applicative (liftA2, pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Foldable (foldr, toList, traverse_)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap, (<$), (<$>))
import safe "base" Data.List (intercalate, reverse)
import safe "base" Data.List.NonEmpty (nonEmpty)
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (uncurry)
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
    { Plugins.installCoreToDos = \opts -> liftA2 install (parseOptions opts) . pure
    }

-- | Reads every option, collecting what went wrong separately from what was
--   understood, so that one bad option cannot hide the next.
--
--   Options are applied in the order they were given, so the last one wins –
--   which is what makes an @OPTIONS_GHC@ pragma override a package-wide
--   @ghc-options@ entry.
parseOpts :: [Plugins.CommandLineOption] -> ([(String, Opts.Error)], Opts)
parseOpts =
  foldr
    ( \opt (errs, opts) ->
        let (name, mvalue) = Opts.process opt
         in either (\e -> ((name, e) : errs, opts)) (errs,) case name of
              "allowRecursion" ->
                (\v -> opts {allowRecursion = v}) <$> Opts.parseBool mvalue
              "ignoreMethodCycles" ->
                (\v -> opts {ignoreMethodCycles = v}) <$> Opts.parseBool mvalue
              "ignoredDecls" ->
                (\v -> opts {ignoredDecls = v <> ignoredDecls opts})
                  <$> Opts.parseRequiringVal (pure . Opts.parseList) "List" mvalue
              "ignoredMethods" ->
                (\v -> opts {ignoredMethods = v <> ignoredMethods opts})
                  <$> Opts.parseRequiringVal (pure . Opts.parseList) "List" mvalue
              _ -> Left Opts.UnknownOption
    )
    ([], defaultOpts)
    -- NOTE: Starting with GHC 8.6, plugin option order is reversed from what is
    --       given on the command line. This restores it, so that the last option
    --       wins.
    . reverse

-- | Stops the compilation if any option was malformed, describing all of them.
--
-- NOTE: `Plugins.errorMsg` prints a message the build then goes on to ignore,
--       so a mistyped option silently did nothing. A `Plugins.GhcException` is
--       reported as the compile error it is.
parseOptions :: [Plugins.CommandLineOption] -> Plugins.CoreM Opts
parseOptions options = do
  dflags <- Plugins.getDynFlags
  let (errs, opts) = parseOpts options
  opts
    <$ traverse_
      ( Plugins.liftIO
          . Plugins.throwGhcExceptionIO
          . Plugins.ProgramError
          . Plugins.showSDoc dflags
          . Plugins.vcat
          . toList
          . fmap (uncurry (Opts.prettyError "NoRecursion"))
      )
      (nonEmpty errs)

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
