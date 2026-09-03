# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog 1.1](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added

- `GHC.Recursion.Core`, `GHC.Recursion.Options` and `GHC.Recursion.Record`,
  extracted from the `no-recursion` plugin so that the analysis can be linked
  into a test-suite rather than reached only by compiling a module and reading
  what GHC printed.

  The analysis is generic in the binder, and takes what it needs to know about
  one — how to name it, and what annotations it carries — as functions. It
  mentions no `DynFlags`, no `NameEnv` and no `CoreM`, so calling it needs no
  compiler session.
