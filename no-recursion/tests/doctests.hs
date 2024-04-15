{-# LANGUAGE Unsafe #-}

module Main (main) where

-- NB: This unqualified module comes from semigroups in GHC <8, and base
--     otherwise.
import safe Data.Semigroup (Semigroup ((<>)))
import safe "base" Data.Function (($))
import safe "base" System.IO (IO)
import "doctest" Test.DocTest (doctest)
import "this" Build_doctests (flags, module_sources, pkgs)

main :: IO ()
main = doctest $ flags <> pkgs <> module_sources
