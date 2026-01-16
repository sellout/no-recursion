# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog 1.1](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.3.0.0] – 2025-11-05

### Added

- Support for GHC 9.12

### Changed

- License from `AGPL-3.0-or-later` to `AGPL-3.0-only WITH
Universal-FOSS-exception-1.0 OR LicenseRef-commercial`

### Removed

- Support for GHC <9.6
- Support for 32-bit Linux

## [0.2.0.0] – 2025-10-30

### Added

- Command-line options to avoid needing to use `Unsafe` `ann` pragmas (#19).

### Removed

- Some dependency bounds were tightened, due to changes in CI coverage.
- Support for GHC 7.10.

## [0.1.2.3] - 2025-03-30

### Added

- Wider dependency bounds to support Stackage (#21).

## [0.1.2.2] - 2024-09-02

### Updated

- Link to published packages in README.
- Add updated versioning documentation to README.
- Include this file in `extra-doc-files`.

### Removed

- `-fpackage-trust` and related options because they cause more trouble than they’re worth

### Fixed

- The license report to reflect the GHC 9.10 build

## 0.1.2.1

**never published**

## [0.1.2.0] - 2024-05-19

### Added

- Support for [GHC 9.10](https://www.haskell.org/ghc/download_ghc_9_10_1.html)

## [0.1.1.0] - 2024-04-16

### Added

- Source annotations (`{-# ANN … "Recursion" #-}`) to enable/disable the plugin
  for limited scopes

## [0.1.0.0] - 2024-04-13

### Added

- Initial release of this package.

[0.3.0.0]: https://github.com/sellout/no-recursion/compare/v0.2.0.0...v0.3.0.0
[0.2.0.0]: https://github.com/sellout/no-recursion/compare/v0.1.2.3...v0.2.0.0
[0.1.2.3]: https://github.com/sellout/no-recursion/compare/v0.1.2.2...v0.1.2.3
[0.1.2.2]: https://github.com/sellout/no-recursion/compare/v0.1.2.0...v0.1.2.2
[0.1.2.0]: https://github.com/sellout/no-recursion/compare/v0.1.1.0...v0.1.2.0
[0.1.1.0]: https://github.com/sellout/no-recursion/compare/v0.1.0.0...v0.1.1.0
[0.1.0.0]: https://github.com/sellout/no-recursion/releases/tag/v0.1.0.0
