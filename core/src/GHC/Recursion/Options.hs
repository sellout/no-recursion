{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- The options that direct the analysis, and the errors that come of reading
-- them badly. Nothing here touches a compiler session; a plugin hands it the
-- strings it was given and gets back either an `Opts` or an `OptError`.
module GHC.Recursion.Options
  ( Opts (..),
    OptError (..),
    defaultOpts,
    parseBoolOpt,
    parseListOpt,
    prettyOptError,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Either (Either (Left))
import safe "base" Data.Foldable (foldr)
import safe "base" Data.Function (($))
import safe "base" Data.Kind (Type)
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (curry)
import "ghc" GHC.Plugins qualified as Plugins

-- | How the analysis should behave on the module being compiled.
--
-- @since 99999
type Opts :: Type
data Opts = Opts
  { -- | Whether recursion is allowed unless something says otherwise.
    --
    -- @since 99999
    allowRecursion :: Bool,
    -- | Whether to overlook the mutual recursion that a class instance and its
    -- default method implementations appear to have.
    --
    -- @since 99999
    ignoreMethodCycles :: Bool,
    -- | Names of declarations whose recursion is not reported.
    --
    -- @since 99999
    ignoredDecls :: [String],
    -- | Names of class methods whose recursion is not reported.
    --
    -- @since 99999
    ignoredMethods :: [String]
  }

-- | The `Opts` we have if no @-fplugin-opts=NoRecursion:@ are provided.
--
-- - recursion is not allowed
-- - recursion cycles between methods is ignored (to avoid a breaking change)
--
-- @since 99999
defaultOpts :: Opts
defaultOpts =
  Opts
    { allowRecursion = False,
      ignoreMethodCycles = True,
      ignoredDecls = [],
      ignoredMethods = []
    }

-- | The ways an option can fail to be understood.
--
-- @since 99999
type OptError :: Type
data OptError
  = -- | An option that needs a value was given without one.
    --
    -- @since 99999
    MissingValue String
  | -- | No option goes by this name.
    --
    -- @since 99999
    UnknownOption String
  | -- | The value doesn’t read as the type the option expects. The first field
    -- names that type.
    --
    -- @since 99999
    UnknownValue String String

-- | Renders an `OptError` the way GHC renders its own option errors.
--
-- @since 99999
prettyOptError :: OptError -> Plugins.SDoc
prettyOptError =
  Plugins.text . \case
    MissingValue opt ->
      "plugin option ‘NoRecursion:" <> opt <> "’ is missing a value"
    UnknownOption name -> "unknown plugin option ‘NoRecursion:" <> name <> "’"
    UnknownValue typ value ->
      "an option for the NoRecursion plugin was expecting a "
        <> typ
        <> " but received ‘"
        <> value
        <> "’"

-- | Reads a `Bool`-valued option.
--
-- @since 99999
parseBoolOpt :: String -> Either OptError Bool
parseBoolOpt = \case
  "true" -> pure True
  "false" -> pure False
  value -> Left $ UnknownValue "Bool" value

-- | Reads a comma-separated option value as the list it denotes.
--
-- @since 99999
parseListOpt :: String -> [String]
parseListOpt =
  foldr
    ( curry $ \case
        (',', elems) -> [] : elems
        (c, []) -> [[c]]
        (c, curr : elems) -> (c : curr) : elems
    )
    []
