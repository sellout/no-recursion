# recursion-analysis

Finding recursion in GHC Core.

This is the analysis behind the [`no-recursion`](../plugin) plugin, separated
from it so that it can be linked into a test-suite and called directly. The
plugin is a thin shell around this: it reads its options, calls
`failOnRecursion`, and aborts the compilation if anything comes back.

## What it does

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

It is generic in the binder. GHC’s `Expr b`, `Bind b` and `Alt b` are all
parameterised, and the traversal never looks inside a `Var` occurrence —
recursion is read off `Rec` binding groups — so the analysis works just as well
at `b ~ String` as at `b ~ CoreBndr`.

It takes what it needs to know about a binder as functions. A plugin passes
`showSDoc dflags . ppr` and a lookup into the module’s annotation environment; a
test passes `id` and a lookup in a list. Nothing here needs a compiler session,
which is the whole point of the separation.

## Versioning

Versions are four components, `A.B.C.D`, and this package follows the same
policy as `no-recursion`, which its README sets out in full. In short: `D` for
changes that can’t break anything, `C` for additions, `B` for breaking changes,
and `A` for widening a dependency to a new major version.
