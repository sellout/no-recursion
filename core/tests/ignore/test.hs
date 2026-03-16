{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Main
  ( main,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Function (($))
import safe "base" Data.Functor (void)
import safe "base" System.IO (IO)
import safe "this" Test.AllowRecursion qualified as AllowRecursion
import "this" Test.AnnModule qualified as AnnModule
import "this" Test.AnnName qualified as AnnName
import safe "this" Test.IgnoreDefaultImpls qualified as IgnoreDefaultImpls
import safe "this" Test.UnannName qualified as UnannName

-- | The test-suite entry point.
--
-- @since 0.1.1
main :: IO ()
main =
  void
    . pure
    . AllowRecursion.nonRecDef
    . AnnModule.nonRecDef
    . AnnName.nonRecDef
    $ UnannName.nonRecDef IgnoreDefaultImpls.Empty
