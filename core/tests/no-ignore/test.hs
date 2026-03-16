{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2025 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Main
  ( main,
  )
where

import "base" Control.Applicative (pure)
import "base" Data.Function (($))
import "base" Data.Functor (void)
import "base" System.IO (IO)
import "this" Test.IgnoreDefaultImpls qualified as IgnoreDefaultImpls

-- | The test-suite entry point.
--
-- @since 0.2.0
main :: IO ()
main = void $ pure IgnoreDefaultImpls.Empty
