# NoRecursion

[![built with garnix](https://img.shields.io/endpoint?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fsellout%2Fno-recursion)](https://garnix.io)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:no-recursion.svg)](https://repology.org/project/haskell:no-recursion/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:no-recursion.svg)](https://repology.org/project/haskell:no-recursion/versions)

A GHC plugin to remove support for recursion

General recursion can be the cause of a lot of problems. This removes recursion from GHC, allowing you to guarantee you’re using other mechanisms, like recursion schemes.

## usage

See [the plugin’s README](./no-recursion/README.md) for usage information.

## development environment

We recommend the following steps to make working in this repository as easy as possible.

### `direnv allow`

This command ensures that any work you do within this repository is done within a consistent reproducible environment. That environment provides various debugging tools, etc. When you leave this directory, you will leave that environment behind, so it doesn’t impact anything else on your system.

### `git config --local include.path ../.config/git/config`

This will apply our repository-specific Git configuration to `git` commands run against this repository. It’s lightweight (you should definitely look at it before applying this command) – it does things like telling `git blame` to ignore formatting-only commits.

## building & development

Especially if you are unfamiliar with the Haskell ecosystem, there is a Nix build (both with and without a flake). If you are unfamiliar with Nix, [Nix adjacent](...) can help you get things working in the shortest time and least effort possible.

### if you have `nix` installed

`nix build` will build and test the project fully.

`nix develop` will put you into an environment where the traditional build tooling works. If you also have `direnv` installed, then you should automatically be in that environment when you're in a directory in this project.

### traditional build

This project is built with [Cabal](https://cabal.readthedocs.io/en/stable/index.html). Individual packages will work with older versions, but ./cabal.package requires Cabal 3.6+.

## versioning

In the absolute, almost every change is a breaking change. This section describes how we mitigate that to provide minor updates and revisions.

Here are some common changes that can have unintended effects:

- adding instances can conflict with downstream orphans,
- adding a module can conflict with a module from another package,
- adding a definition to an existing module can conflict if there are unqualified imports, and
- even small bugfixes can introduce breaking changes where downstream depended on the broken results.

To mitigate some of those issues for versioning, we assume the following usage:

- modules should be imported using `PackageImports`, so that adding modules is a _minor_ change;
- modules should be imported qualified, so that adding definitions is a _minor_ change;
- adding instances can't be mitigated in the same way, and it's not uncommon for downstream libraries to add orphans instances when they're omitted from upstream libraries. However, since these conflicts can only happen via direct dependencies, and represent an explicit downstream workaround, it’s reasonable to expect a quick downstream update to remove or conditionalize the workaround. So, this is considered a _minor major_ change;
- deprecation is considered a _revision_ change, however it will often be paired with _minor_ changes. `-Werror` can cause this to fail, but published libraries shouldn't be compiled with `-Werror`.
