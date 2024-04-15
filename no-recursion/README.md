# NoRecursion plugin

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

## comparisons

Other projects similar to this one, and how they differ.

### [WartRemover](https://www.wartremover.org/)

WartRemover is a Scala linting tool. [A `Recursion` wart](https://www.wartremover.org/doc/warts.html#recursion) was added in 2017, and I’ve been meaning to write this plugin ever since. It only took seven years to find a few hours to make it happen …
