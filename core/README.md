# recursion-analysis

[![Hackage Version](https://img.shields.io/hackage/v/recursion-analysis)](https://hackage.haskell.org/package/recursion-analysis)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:recursion-analysis.svg)](https://repology.org/project/haskell:recursion-analysis/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:recursion-analysis.svg)](https://repology.org/project/haskell:recursion-analysis/versions)

Finding recursion in GHC Core.

This is the analysis behind [the `NoRecursion` plugin](../plugin), separated from it so that it can be linked into a test-suite and called directly. The plugin is a thin shell around this: it reads its options, calls `failOnRecursion`, and aborts the compilation if anything comes back.

## usage

- [user guide](https://sellout.github.io/no-recursion)
- [comparisons to similar projects](#comparisons)

`failOnRecursion` walks a list of Core bindings and reports every recursive
binding group that the options don’t excuse, along with the chain of binders it
was found under.

```haskell
failOnRecursion ::
  (b -> String) ->      -- ^ name a binder
  (b -> [String]) ->    -- ^ the annotations on a binder
  [String] ->           -- ^ the annotations on the module
  Opts ->
  [Plugins.Bind b] ->
  Either (NonEmpty (RecursionRecord b)) ()
```

Two things are worth noticing in that signature.

It’s generic in the binder. GHC’s `Expr b`, `Bind b` and `Alt b` are all parameterized, and the traversal never looks inside a `Var` occurrence — recursion is read off `Rec` binding groups – so the analysis works just as well at `b ~ String` as at `b ~ CoreBndr`.

It takes what it needs to know about a binder as functions. A plugin passes `showSDoc dflags . ppr` and a lookup into the module’s annotation environment; a test passes `id` and a lookup in a list. Nothing here needs a compiler session, which is the whole point of the separation.

### licensing

This package is licensed under [The GNU AGPL 3.0 only, with some alternatives](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20no-recursion).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

### versioning

This project largely follows a strict variation of the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP). See [the specifics](https://sellout.github.io/flaky/haskell/strict-PVP.html).

## contributing

- [contributor guide](https://sellout.github.io/flaky/CONTRIBUTING/haskell.html)

## comparisons

Other projects similar to this one, and how they differ.
