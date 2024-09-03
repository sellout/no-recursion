# NoRecursion plugin

[![Packaging status](https://repology.org/badge/tiny-repos/haskell:no-recursion.svg)](https://repology.org/project/haskell:no-recursion/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:no-recursion.svg)](https://repology.org/project/haskell:no-recursion/versions)

A GHC plugin to remove support for recursion

General recursion can be the cause of a lot of problems. This removes recursion from GHC, allowing you to guarantee you’re using other mechanisms, like recursion schemes.

## usage

Add `no-recursion` to your build dependencies.

Add `-fplugin NoRecursion` to your GHC options. This can be done per-module with

```haskell
{-# OPTIONS_GHC -fplugin NoRecursion #-}
```

Now, any recursion in that module will result in a compilation failure.

**NB**: This won’t prevent you from using recursive functions imported from other modules, but inlined definitions from other modules _will_ be checked.

### allowing some recursion

NoRecursion supports two source annotations: `"Recursion"` and `"NoRecursion"`.

You can re-enable recursion for individual top-level names like

```haskell
recDef :: a -> b
recDef = myRecDef
{-# ANN recDef "Recursion" #-}
```

Or you can re-enable recursion for an entire module with

```haskell
{-# ANN module "Recursion" #-}
```

And then you can re-disable recursion for individual names with

```haskell
nonRecDef :: a -> a
nonRecDef = id
{-# ANN nonRecDef "NoRecursion" #-}
```

If both '"Recursion"' and `"NoRecursion"` annotations exist on the same name (or module), it’s treated as `NoRecursion`.

**NB**: If multiple names are mutually recursive, then they must all have recursion enabled to avoid being flagged by the plugin.

`ANN` has some caveats:

- `ANN` isn’t allowed by [Safe Haskell](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/safe_haskell.html), so any module that uses it will be inferred as `Unsafe`.
- If you enable [the `OverloadedStrings` language extension](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/overloaded_strings.html), you will have to specify the type in the annotation, like

  ```haskell
  {-# ANN module "Recursion" :: String #-}
  ```

For more about how to use annotations, see [the GHC User’s Guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/extending_ghc.html#source-annotations).

## versioning

This project largely follows the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP), but is more strict in some ways.

The version always has four components, `A.B.C.D`. The first three correspond to those required by PVP, while the fourth matches the “patch” component from [Semantic Versioning](https://semver.org/).

Here is a breakdown of some of the constraints:

### sensitivity to additions to the API

PVP recommends that clients follow [these import guidelines](https://wiki.haskell.org/Import_modules_properly) in order that they may be considered insensitive to additions to the API. However, this isn’t sufficient. We expect clients to follow these additional recommendations for API insensitivity

If you don’t follow these recommendations (in addition to the ones made by PVP), you should ensure your dependencies don’t allow a range of `C` values. That is, your dependencies should look like

```cabal
yaya >=1.2.3 && <1.2.4
```

rather than

```cabal
yaya >=1.2.3 && <1.3
```

#### use package-qualified imports everywhere

If your imports are [package-qualified](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/package_qualified_imports.html?highlight=packageimports#extension-PackageImports), then a dependency adding new modules can’t cause a conflict with modules you already import.

#### avoid orphans

Because of the transitivity of instances, orphans make you sensitive to your dependencies’ instances. If you have an orphan instance, you are sensitive to the APIs of the packages that define the class and the types of the instance.

One way to minimize this sensitivity is to have a separate package (or packages) dedicated to any orphans you have. Those packages can be sensitive to their dependencies’ APIs, while the primary package remains insensitive, relying on the tighter ranges of the orphan packages to constrain the solver.

### transitively breaking changes (increments `A`)

#### removing a type class instance

Type class instances are imported transitively, and thus changing them can impact packages that only have your package as a transitive dependency.

#### widening a dependency range with new major versions

This is a consequence of instances being transitively imported. A new major version of a dependency can remove instances, and that can break downstream clients that unwittingly depended on those instances.

A library _may_ declare that it always bumps the `A` component when it removes an instance (as this policy dictates). In that case, only `A` widenings need to induce `A` bumps. `B` widenings can be `D` bumps like other widenings, Alternatively, one may compare the APIs when widening a dependency range, and if no instances have been removed, make it a `D` bump.

### breaking changes (increments `B`)

#### restricting an existing dependency’s version range in any way

Consumers have to contend not only with our version bounds, but also with those of other libraries. It’s possible that some dependency overlapped in a very narrow way, and even just restricting a particular patch version of a dependency could make it impossible to find a dependency solution.

#### restricting the license in any way

Making a license more restrictive may prevent clients from being able to continue using the package.

#### adding a dependency

A new dependency may make it impossible to find a solution in the face of other packages dependency ranges.

### non-breaking changes (increments `C`)

#### adding a module

This is also what PVP recommends. However, unlike in PVP, this is because we recommend that package-qualified imports be used on all imports.

### other changes (increments `D`)

#### widening a dependency range for non-major versions

This is fairly uncommon, in the face of `^>=`-style ranges, but it can happen in a few situations.

#### deprecation

**NB**: This case is _weaker_ than PVP, which indicates that packages should bump their major version when adding `deprecation` pragmas.

We disagree with this because packages shouldn’t be _publishing_ with `-Werror`. The intent of deprecation is to indicate that some API _will_ change. To make that signal a major change itself defeats the purpose. You want people to start seeing that warning as soon as possible. The major change occurs when you actually remove the old API.

Yes, in development, `-Werror` is often (and should be) used. However, that just helps developers be aware of deprecations more immediately. They can always add `-Wwarn=deprecation` in some scope if they need to avoid updating it for the time being.

## licensing

This package is licensed under [The GNU AGPL 3.0 or later](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20no-recursion).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

## comparisons

Other projects similar to this one, and how they differ.

### [WartRemover](https://www.wartremover.org/)

WartRemover is a Scala linting tool. [A `Recursion` wart](https://www.wartremover.org/doc/warts.html#recursion) was added in 2017, and I’ve been meaning to write this plugin ever since. It only took seven years to find a few hours to make it happen …
