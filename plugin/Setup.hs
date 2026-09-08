-- __NB__: `custom-setup` doesn’t have any way to specify extensions or options,
--         so any we want need to be specified here.
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Weverything #-}
-- Warns even when `Unsafe` is explicit, not inferred. See
-- https://gitlab.haskell.org/ghc/ghc/-/issues/16689
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main (main) where

-- WAIT: These modules are missing from Cabal 3.8’s re-export list, so we can’t
--       qualify them. Add the package qualifiers once we no longer support GHC
--       9.4 (which bundles Cabal 3.8).
import safe Distribution.Pretty (prettyShow)
import safe Distribution.Types.BuildInfo (BuildInfo)
import "Cabal" Distribution.Simple
  ( defaultMainWithHooks,
    hookedPreProcessors,
  )
import "Cabal" Distribution.Simple.LocalBuildInfo
  ( ComponentLocalBuildInfo,
    LocalBuildInfo,
    compiler,
    componentInternalDeps,
    hostPlatform,
    withPackageDB,
    withPrograms,
  )
import "Cabal" Distribution.Simple.PreProcess
  ( PreProcessor,
    platformIndependent,
    ppUnlit,
    runPreProcessor,
  )
import "Cabal" Distribution.Simple.Program
  ( ghcProgram,
    lookupProgram,
    programPath,
  )
import "Cabal" Distribution.Simple.Program.GHC
  ( ghcOptPackageDBs,
    renderGhcOptions,
  )
import "Cabal" Distribution.Simple.Utils (rewriteFileEx)
import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Bool (Bool (False))
import safe "base" Data.Char (Char)
import safe "base" Data.Foldable (foldMap)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap)
import safe "base" Data.List (intercalate)
import safe "base" Data.Maybe (Maybe, listToMaybe, maybe)
import safe "base" Data.Monoid (mempty)
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String, fromString)
import safe "base" System.IO (FilePath, IO)
import "cabal-doctest" Distribution.Extra.Doctest (doctestsUserHooks)
import safe "filepath" System.FilePath ((</>))

-- | The suffix of the placeholder source file that stands in for the generated
--   module, and so decides which component gets it.
--
--   Cabal drives preprocessing from the module lists: for each module named in
--   a component it looks through that component’s `hs-source-dirs` for a file
--   named after the module with a registered suffix, and only then runs the
--   handler. So a placeholder has to exist for `Build_fixtures`, even though
--   nothing ever reads it.
--
--   The convention is documented for the built-in preprocessors, though not
--   for the ones a `UserHooks` adds:
--   https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#modules-and-preprocessors
fixtureSuffix :: String
fixtureSuffix = "invocation"

main :: IO ()
main =
  let doctestHooks = doctestsUserHooks "doctests"
   in defaultMainWithHooks
        doctestHooks
          { hookedPreProcessors =
              (fromString fixtureSuffix, fixtureInvocation)
                : hookedPreProcessors doctestHooks
          }

-- | Writes the module the “recursion-errors” suite reads to learn how to invoke
--   GHC: the compiler this build used, and the flags that load the library
--   component it just produced.
fixtureInvocation ::
  BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
fixtureInvocation _buildInfo lbi clbi =
  -- WAIT: `ppUnlit` is borrowed only for its `ppOrdering` default (which is a
  --       field added in Cabal 3.8.1). Build the `PreProcessor` explicitly once
  --       we no longer support GHC 9.2 (which bundles Cabal 3.6).
  ppUnlit
    { platformIndependent = False,
      runPreProcessor = \_source (outDir, outFile) verbosity ->
        rewriteFileEx verbosity (outDir </> outFile)
          . moduleSource (ghcPath lbi)
          $ pluginFlags lbi clbi
    }

-- | The compiler this build used, rather than whichever one is on @PATH@ when
--   the suite runs — they can be different, and a plugin can only be loaded by
--   the GHC it was built for.
ghcPath :: LocalBuildInfo -> FilePath
ghcPath = maybe "ghc" programPath . lookupProgram ghcProgram . withPrograms

-- | Flags that make GHC load the library this build produced.
pluginFlags :: LocalBuildInfo -> ComponentLocalBuildInfo -> [String]
pluginFlags lbi clbi =
  "-package-env=-"
    : renderGhcOptions
      (compiler lbi)
      (hostPlatform lbi)
      mempty {ghcOptPackageDBs = withPackageDB lbi}
      <> foldMap (("-package-id" :) . pure) (libraryUnit clbi)

-- | The unit id of the library this build produced.
libraryUnit :: ComponentLocalBuildInfo -> Maybe String
libraryUnit = fmap prettyShow . listToMaybe . componentInternalDeps

-- | The generated module.
moduleSource :: FilePath -> [String] -> String
moduleSource ghc flags =
  foldMap
    (<> "\n")
    [ "{-# LANGUAGE Safe #-}",
      "",
      "-- |",
      "-- Copyright: 2026 Greg Pfeil",
      "-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary",
      "--",
      "-- How to invoke GHC so that it loads the plugin this build produced.",
      "--",
      "-- Generated by @Setup.hs@.",
      "module Build_fixtures (flags, ghc) where",
      "",
      "import safe \"base\" Data.String (String)",
      "import safe \"base\" System.IO (FilePath)",
      "",
      "-- | The compiler this package was built with.",
      "--",
      "-- @since 99999",
      "ghc :: FilePath",
      "ghc = " <> literal ghc,
      "",
      "-- | The flags that make that compiler load the plugin just built.",
      "--",
      "-- @since 99999",
      "flags :: [String]",
      "flags = [" <> intercalate ", " (fmap literal flags) <> "]"
    ]

-- | A Haskell string literal for a path, which may contain a backslash or a
--   quotation mark.
literal :: String -> String
literal string = "\"" <> foldMap escape string <> "\""
  where
    escape :: Char -> String
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape character = [character]
