# recursion-analysis

[![Hackage Version](https://img.shields.io/hackage/v/recursion-analysis)](https://hackage.haskell.org/package/recursion-analysis)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:recursion-analysis.svg)](https://repology.org/project/haskell:recursion-analysis/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:recursion-analysis.svg)](https://repology.org/project/haskell:recursion-analysis/versions)

Finding recursion in GHC Core.

This is the analysis behind [the `NoRecursion` plugin](../plugin), separated from it so that it can be linked into a test-suite and called directly.

## usage

- [user guide](https://sellout.github.io/no-recursion)
- [comparisons to similar projects](#comparisons)

### licensing

This package is licensed under [The GNU AGPL 3.0 only, with some alternatives](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20no-recursion).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

### versioning

This project largely follows a strict variation of the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP). See [the specifics](https://sellout.github.io/flaky/haskell/strict-PVP.html).

## contributing

- [contributor guide](https://sellout.github.io/flaky/CONTRIBUTING/haskell.html)

## comparisons

Other projects similar to this one, and how they differ.
