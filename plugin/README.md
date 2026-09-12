# NoRecursion plugin

[![Hackage Version](https://img.shields.io/hackage/v/no-recursion)](https://hackage.haskell.org/package/no-recursion)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:no-recursion.svg)](https://repology.org/project/haskell:no-recursion/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:no-recursion.svg)](https://repology.org/project/haskell:no-recursion/versions)

A GHC plugin to remove support for recursion

General recursion can be the cause of a lot of problems. This removes recursion from GHC, allowing you to guarantee you’re using other mechanisms, like recursion schemes.

## usage

- [user guide](https://sellout.github.io/no-recursion)
- [comparisons to similar projects](#comparisons)

Add `no-recursion` to your build dependencies.

Add `-fplugin NoRecursion` to your GHC options. This can be done per-module with

```haskell
{-# options_ghc -fplugin NoRecursion #-}
```

Now, any recursion in that module will result in a compilation failure.

**NB**: This won’t prevent you from using recursive functions imported from other modules, but inlined definitions from other modules _will_ be checked.

### allowing some recursion

The recommended way to re-enable recursion at the module level is to add

```haskell
{-# options_ghc -fplugin-opt=NoRecursion:allowRecursion=True #-}
```

at the beginning of the file.

If you want to re-enable it for specific definitions, the best way is to use

```haskell
{-# options_ghc -fplugin-opt=NoRecursion:ignoredDecls=recDef #-}

recDef :: a -> b
recDef = recDef
```

You can also do it with a source annotation

```haskell
recDef :: a -> b
recDef = recDef
{-# ann recDef "Recursion" #-}
```

Unfortunately, the `ann` pragma isn’t allowed by [Safe Haskell](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/safe_haskell.html), so any module that uses it will be inferred as `Unsafe`. `-fplugin-opt NoRecursion:allowRecursion=True` doesn’t have that issue.

NoRecursion supports two [source annotations](https://downloads.haskell.org/ghc/latest/docs/users_guide/extending_ghc.html#source-annotations): `"Recursion"` and `"NoRecursion"`.

You can re-enable recursion for an entire module with

```haskell
{-# ann module "Recursion" #-}
```

And then you can re-disable recursion for individual names with

```haskell
nonRecDef :: a -> a
nonRecDef = id
{-# ann nonRecDef "NoRecursion" #-}
```

If both '"Recursion"' and `"NoRecursion"` annotations exist on the same name (or module), it’s treated as `NoRecursion`.

**NB**: If multiple names are mutually recursive, then they must all have recursion enabled to avoid being flagged by the plugin.

`ann` has some caveats:

- If you enable [the `OverloadedStrings` language extension](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/overloaded_strings.html), you will have to specify the type in the annotation, like

  ```haskell
  {-# ann module "Recursion" :: String #-}
  ```

### plugin options

The plugin currently supports four options

- `allowRecursion`: (`True`|`False`) whether to allow recursion by default. As mentioned above, this is the best way to re-enable recursion for a single module, but you can do the reverse and specify `allowRecursion=True` globally, then use `allowRecursion=False` per-module.

- `ignoreMethodCycles`: (`True`|`False`) whether to ignore cycles between method definitions.the method level. This crops up a lot with errors about things like `$csconcat`.

- `ignoredMethods`: (list of method names) ignores the named methods. Useful for silencing errors about default method definitions.

- `ignoredDecls`: (list of decl names) ignores the named declarations. This is good to put at the top of a module where you have intentionally written a recursive definition.

An option that takes a value is written `-fplugin-opt NoRecursion:‹name›=‹value›` — GHC’s own `NoRecursion:` prefix, then the option name, then `=`, then the value. An option that takes no value is just the name.

If a `Bool` option is given more than once, the last one wins, so an `options_ghc` pragma at the top of a module overrides an entry in your Cabal file’s `ghc-options`. `ignoredDecls` and `ignoredMethods` accumulate instead: every occurrence adds to the list, and there is no way to take a name back off it. An option that isn’t recognised, or that’s missing a value, stops the compilation rather than being ignored.

### suggestions

#### `in $csconcat, the following bindings were recursive: go1`

This particular message occurs when you define a `Semigroup` instance that didn’t have an explicit `sconcat` implementation. The default definition is recursive, and `NoRecursion` catches that. Similar messages occur with default definitions for other classes as well.

You can’t apply `ann` to methods, so here are some ways to get around this issue:

1. write an explicit non-recursive definition, or
2. add `{-# options_ghc -fplugin-opt=NoRecursion:ignoredMethods=sconcat #-}` to the top of the module, which ignores this method module-wide.

Unfortunately, because `sconcat` (and `mconcat`) require lazy lists (`[]`), it’s not possible to write a total definition for these.

### mitigating [dependency hell](https://en.wikipedia.org/wiki/Dependency_hell)

As NoRecursion is effectively a linter, you don’t have to depend on it in every case (although, be careful, because different GHC versions may catch (or induce) different occurrences of recursion).

You can conditionalize the use of NoRecursion by adding the following (with a suitable replacement for `_`) to the stanzas in your Cabal file:

```cabal
  if _
    build-depends:
      no-recursion ^>= {x.y.z},
    ghc-options:
      -fplugin=NoRecursion
```

If the plugin isn’t enabled, any `-fplugin-opt NoRecursion:…` elsewhere will be ignored.

Here are a couple concrete situations where this is useful.

#### you support a some environment that NoRecursion doesn’t

```cabal
  if impl(ghc >= 9.6.1) && impl(ghc < 9.14.1) && !arch(i386)
```

With the preceding condition, NoRecursion will only be used with GHC 9.6.1–9.12 and on architectures that aren’t i386 (32-bit).

We would love to have NoRecursion work in all your environments, so please [open an issue](https://github.com/sellout/no-recursion/issues/new?title=Add+support+for+&labels=dependencies,enhancement) if you find yourself using this approach. Since it’s a compiler plugin, it’s more sensitive to GHC changes than most code, so just ignoring dependency bounds is less likely to work.

#### you want to get out of consumers’ way

A common situation is depending on a version that isn’t widely available (this can happen with Stackage or maybe it’s an unpublished revision that you’ve added as a `source-repository-package`).

In this case, you can define a flag in your Cabal file

```cabal
flag verify-no-recursion
  description:
    Compile with "NoRecursion" enabled. This is intended for developers of this
    package.
  default: False
  manual: True
```

And then conditionalize on that

```cabal
  if flag(verify-no-recursion)
```

In cabal.project, you should also add

```cabal
flags:
  +verify-no-recursion
```

which ensures that the flag doesn’t get automatically turned off when doing local development. You don’t want to discover that you had the `no-recursion` bounds set incorrectly only after a user complains that they can’t compile your library because of recursion errors.

If you’re using Stack, you can achieve the same thing with

```yaml
flags:
  local-package:
    verify-no-recursion: true
  another-local-package:
    verify-no-recursion: true
```

Note that with Stack you need to set the flag for each package in your project.

You can see an example of this (with Cabal) in the [duoids](https://github.com/sellout/duoids/blob/6de6468d173fdb8b95db3789d65984289b7b42d5/core/duoids.cabal#L64-L70) project.

### licensing

This package is licensed under [The GNU AGPL 3.0 only](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20no-recursion).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

### versioning

This project largely follows a strict variation of the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP). See [the specifics](https://sellout.github.io/flaky/haskell/strict-PVP.html).

## contributing

- [contributor guide](https://sellout.github.io/flaky/CONTRIBUTING/haskell.html)

## comparisons

Other projects similar to this one, and how they differ.

### [WartRemover](https://www.wartremover.org/)

WartRemover is a Scala linting tool. [A `Recursion` wart](https://www.wartremover.org/doc/warts.html#recursion) was added in 2017, and I’ve been meaning to write this plugin ever since. It only took seven years to find a few hours to make it happen …
