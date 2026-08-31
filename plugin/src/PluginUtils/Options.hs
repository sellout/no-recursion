{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- The options that direct the analysis, and the errors that come of reading
-- them badly. Nothing here touches a compiler session; a plugin hands it the
-- strings it was given and gets back either an `Opts` or an `Error`.
module PluginUtils.Options
  ( Error (..),
    parseBool,
    parseList,
    parseRequiringVal,
    prettyError,
    process,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Bifunctor (second)
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Either (Either (Left))
import safe "base" Data.Foldable (foldr)
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Kind (Type)
import safe "base" Data.List (drop, elemIndex, splitAt)
import safe "base" Data.Maybe (Maybe (Nothing), maybe)
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (curry)
import "ghc" GHC.Plugins qualified as Plugins

-- | The ways an option can fail to be understood.
--
-- @since 99999
type Error :: Type
data Error
  = -- | No option goes by this name.
    --
    -- @since 99999
    UnknownOption
  | -- | An option that needs a value was given without one.
    --
    -- @since 99999
    MissingValue String
  | -- | The value doesn’t read as the type the option expects. The first field
    -- names that type.
    --
    -- @since 99999
    UnknownValue String String
  | MissingRequiredOption

-- | Renders an `Error` the way GHC renders its own option errors.
--
-- @since 99999
prettyError :: String -> String -> Error -> Plugins.SDoc
prettyError pluginName optionName =
  let option = pluginName <> ":" <> optionName
   in Plugins.text . \case
        UnknownOption ->
          "unknown plugin option ‘" <> option <> "’"
        MissingValue typ ->
          "plugin option ‘"
            <> option
            <> "’ was expecting a "
            <> typ
            <> " but had no value"
        UnknownValue typ value ->
          "plugin option ‘"
            <> option
            <> "’ was expecting a "
            <> typ
            <> " but received ‘"
            <> value
            <> "’"
        MissingRequiredOption -> "plugin option ‘" <> option <> "’ is required"

-- | Splits one option into its name and, if it has one, its value, on the
--   first @=@ — so an option is written @-fplugin-opt Plugin:name=value@. GHC
--   has already taken the @Plugin:@ prefix off by this point, and a @=@
--   separator leaves the rest of the option free to contain @:@.
--
-- @since 99999
process :: Plugins.CommandLineOption -> (String, Maybe String)
process opt =
  maybe (opt, Nothing) (second (pure . drop 1) . flip splitAt opt) $
    elemIndex '=' opt

-- | Reads an option that has no meaning without a value, so that every such
--   option reports a missing one the same way. @typ@ names what was expected.
--
-- @since 99999
parseRequiringVal ::
  (String -> Either Error a) -> String -> Maybe String -> Either Error a
parseRequiringVal parser typ = maybe (Left $ MissingValue typ) parser

-- | Reads a `Bool`-valued option.
--
-- @since 99999
parseBool :: Maybe String -> Either Error Bool
parseBool = maybe (pure True) \case
  "true" -> pure True
  "false" -> pure False
  value -> Left $ UnknownValue "Bool" value

-- | Reads a comma-separated option value as the list it denotes.
--
-- @since 99999
parseList :: String -> [String]
parseList =
  foldr
    ( curry $ \case
        (',', elems) -> [] : elems
        (c, []) -> [[c]]
        (c, curr : elems) -> (c : curr) : elems
    )
    []
