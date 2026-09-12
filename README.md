# NoRecursion

[![GitHub CI](https://github.com/sellout/no-recursion/actions/workflows/build.yml/badge.svg)](https://github.com/sellout/no-recursion/actions/workflows/build.yml)
[![Nix CI](https://nix-ci.com/badge/gh:sellout:no-recursion)](https://nix-ci.com/gh:sellout:no-recursion)
[![Project Manager](https://img.shields.io/badge/%20-Project%20Manager-%235277C3?logo=nixos&labelColor=%23cccccc)](https://sellout.github.io/project-manager/)

A GHC plugin to remove support for recursion

General recursion can be the cause of a lot of problems. This removes recursion from GHC, allowing you to guarantee you’re using other mechanisms, like recursion schemes.

## usage

See [the plugin’s README](./plugin/README.md) for usage information.

## contributing

- [contributor guide](https://sellout.github.io/flaky/CONTRIBUTING/haskell.html)

## comparisons

See [the plugin’s README](./plugin/README.md#comparisons) for comparisons with other similar projects.
