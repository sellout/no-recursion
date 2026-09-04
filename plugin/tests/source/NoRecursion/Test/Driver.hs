{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Runs Haskell source through GHC in-process, as far as the desugarer, and
-- hands the Core it produces to the analysis.
module NoRecursion.Test.Driver
  ( Analysis (..),
    Outcome (..),
    analyze,
    findLibdir,
  )
where

import safe "base" Control.Applicative (empty, pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Exception (IOException, try)
import safe "base" Control.Monad ((<=<), (=<<))
import safe "base" Data.Bool (Bool (True), otherwise)
import safe "base" Data.Char (isSpace)
import safe "base" Data.Either (Either, either)
import safe "base" Data.Eq (Eq ((==)))
import safe "base" Data.Foldable (length, toList)
import safe "base" Data.Function (const, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Kind (Type)
import safe "base" Data.List (dropWhileEnd, isInfixOf)
import safe "base" Data.Maybe (Maybe, maybe)
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (fst, snd)
import safe "base" Data.Version (showVersion)
import safe "base" System.Environment (lookupEnv)
import safe "base" System.IO (FilePath, IO, writeFile)
import safe "base" System.IO.Error (catchIOError)
import safe "base" System.Info (fullCompilerVersion)
import safe "base" Text.Show (Show, show)
import "directory" System.Directory
  ( createDirectoryIfMissing,
    getTemporaryDirectory,
    removeDirectoryRecursive,
  )
import safe "filepath" System.FilePath ((</>))
import "ghc" GHC qualified
import "ghc" GHC.Plugins qualified as Plugins
import "ghc" GHC.Unit.Types qualified as GHC.Unit
import safe "no-recursion" NoRecursion.Internal (Opts, failOnRecursion)
import "process" System.Process (readProcess)
import safe "recursion-analysis" GHC.Recursion (Record)

-- |
--
--  __TODO__: Get this into ghc-compat (once we depend on that here).
guessTarget ::
  (GHC.GhcMonad m) =>
  FilePath ->
  Maybe GHC.Unit.UnitId ->
  Maybe GHC.Phase ->
  m GHC.Target
#if MIN_VERSION_ghc(9, 4, 0)
guessTarget = GHC.guessTarget
#else
guessTarget file _ = GHC.guessTarget file
#endif

-- | What the two analysis phases found, normalised to names so that a case can
--   write what it expects.
--
-- @since 99999
type Analysis :: Type
data Analysis = Analysis
  { -- | What the renamer phase found. Always empty until there is a renamer
    -- phase to ask.
    --
    -- @since 99999
    renamedRecords :: [Record String],
    -- | What the Core phase found.
    --
    -- @since 99999
    coreRecords :: [Record String]
  }
  deriving stock (Eq, Show)

-- | Whether GHC got far enough to have an answer.
--
-- @since 99999
type Outcome :: Type
data Outcome
  = -- | GHC could not get the module as far as Core.
    --
    -- @since 99999
    Failed String
  | -- | It did, and this is what the analysis says about the result.
    --
    -- @since 99999
    Analyzed Analysis
  deriving stock (Eq, Show)

-- | A @libdir@ for a GHC matching the one this suite was built against, or
--   `Nothing` if there isn’t one, in which case the suite skips rather than
--   failing.
--
-- @since 99999
findLibdir :: IO (Maybe FilePath)
findLibdir =
  maybe
    ( maybe
        (pure empty)
        ( \v ->
            if v == showVersion fullCompilerVersion
              then ghc ["--print-libdir"]
              else pure empty
        )
        =<< ghc ["--numeric-version"]
    )
    (pure . pure)
    =<< lookupEnv "NO_RECURSION_TEST_GHC_LIBDIR"
  where
    ghc args =
      either (const empty) (pure . dropWhileEnd isSpace)
        <$> (try (readProcess "ghc" args "") :: IO (Either IOException String))

-- | Compiles @src@ as far as Core and reports what the analysis makes of it.
--
--   The module must be called @M@. It must not carry a @-fplugin@ pragma: this
--   session would try to load the plugin from its own package database, which
--   is exactly the problem an in-process driver exists to avoid.
--
-- @since 99999
analyze :: FilePath -> Opts -> String -> IO Outcome
analyze libdir opts src
  | "-fplugin" `isInfixOf` src =
      pure $ Failed "a case for this suite must not load a plugin"
  | otherwise = do
      tmp <- getTemporaryDirectory
      let dir = tmp </> "no-recursion-source"
      -- The scratch directory may not exist yet, and that’s not a failure.
      ignoreIOExceptions $ removeDirectoryRecursive dir
      createDirectoryIfMissing True dir
      let file = dir </> "M.hs"
      writeFile file src
      GHC.runGhc (pure libdir) do
        flags <- GHC.getSessionDynFlags
        _ <- GHC.setSessionDynFlags flags {GHC.importPaths = [dir]}
        GHC.setTargets . pure =<< guessTarget file empty empty
        summaries <- GHC.mgModSummaries <$> GHC.depanal [] True
        case summaries of
          [summary] -> do
            guts <-
              fmap GHC.coreModule . (GHC.desugarModule <=< GHC.typecheckModule)
                =<< GHC.parseModule summary
            pure . Analyzed $
              Analysis
                { renamedRecords = [],
                  coreRecords = coreRecordsOf flags opts guts
                }
          _ ->
            pure . Failed $
              "expected exactly one module, got " <> show (length summaries)

-- |
--
--  __NOTE__: Stolen from `directory`.
ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions = (`catchIOError` \_ -> pure ())

coreRecordsOf :: Plugins.DynFlags -> Opts -> Plugins.ModGuts -> [Record String]
coreRecordsOf flags opts guts =
  let anns =
        Plugins.deserializeAnns Plugins.deserializeWithData . Plugins.mkAnnEnv $
          Plugins.mg_anns guts
      modAnns =
        Plugins.lookupWithDefaultModuleEnv (fst anns) [] $
          Plugins.mg_module guts
      annsOf =
        Plugins.lookupWithDefaultUFM_Directly (snd anns) [] . Plugins.getUnique
   in fmap Plugins.getOccString
        <$> either
          toList
          (const [])
          ( failOnRecursion
              (Plugins.showSDoc flags . Plugins.ppr)
              annsOf
              modAnns
              opts
              (Plugins.mg_binds guts)
          )
