### All available options for this file are listed in
### https://sellout.github.io/project-manager/options.xhtml
{
  config,
  lib,
  self,
  ...
}: {
  project = {
    name = "no-recursion";
    summary = "A GHC plugin to remove support for recursion";
    file = let
      ## Cabal requires many files to exist at the package level, rather than
      ## the repo level. This makes copies of the individual files into the
      ## package directory.
      ##
      ## TODO: Move something like this to Flaky.
      perPackageFiles = dir: {
        "${dir}/LICENSE".source = ../../LICENSE;
        "${dir}/LICENSE.AGPL-3.0-only".source = ../../LICENSE.AGPL-3.0-only;
        "${dir}/LICENSE.Universal-FOSS-exception-1.0".source =
          ../../LICENSE.Universal-FOSS-exception-1.0;
        "${dir}/LICENSE.proprietary".source = ../../LICENSE.proprietary;
        ## We might want to put this somewhere else (like .config/henforcer/),
        ## but that isn’t currently an option, because of flipstone/henforcer#7.
        "${dir}/henforcer.toml".text =
          lib.pm.generators.toTOML {} {
            globalSection = {};
            sections = {
              forAnyModule = {
                ## doesn’t yet support nested attr sets
                # allowedAliasUniqueness.allAliasesUniqueExcept = [];
                maximumExportsPlusHeaderUndocumented = 0;
                maximumExportsWithoutSince = 0;
                moduleHeaderCopyrightMustExistNonEmpty = true;
                ## We want to require a description, but just a “normal”
                ## description, not the header field.
                moduleHeaderDescriptionMustExistNonEmpty = false;
                moduleHeaderLicenseMustExistNonEmpty = true;
              };
            };
          }
          ## NB: `toTOML` is really just an INI generator, so it can’t handle a
          ##     lot of syntax. This tacks some bits onto the end that the INI
          ##     generator does’t like.
          + ''
            # Exclude auto-generated `Paths` module
            [[forPatternModules]]
            pattern = "Paths_*"
            [forPatternModules.rulesToIgnore]
            all = true

            # Exclude auto-generated `Build_doctests` module
            [[forSpecifiedModules]]
            module = "Build_doctests"
            [forSpecifiedModules.rulesToIgnore]
            all = true
          '';
      };
    in
      perPackageFiles "core";
  };

  imports = [./hlint.nix];

  ## CI
  ## FIXME: Shouldn’t need `mkForce` here (or to duplicate the base contexts).
  ##        Need to improve module merging.
  services.github.settings.branches.main.protection.required_status_checks.contexts =
    lib.mkForce
    ([
        "All Garnix checks"
        "check-bounds"
        "check-licenses"
      ]
      ++ lib.concatMap (sys:
        lib.concatMap (ghc:
          ## Don’t add `exclude`d matrix entries to the required list
          ##
          ## TODO: Make this less manual (like the `include` component).
            if
              ## GHC before 8.4 needs an older Ubuntu
              lib.versionOlder ghc "8.4"
              && sys == "ubuntu-24.04"
              ## GHC doesn’t support ARM before GHC 9.2.
              || lib.versionOlder ghc "9.2"
              && builtins.elem sys ["macos-15" "ubuntu-24.04-arm"]
              ## GHC 9.2.1 relied on libnuma at runtime for aarch64
              || ghc == "9.2.1" && sys == "ubuntu-24.04-arm"
            then []
            else [
              "build (${ghc}, ${sys})"
              "build (--prefer-oldest, ${ghc}, ${sys})"
            ])
        self.lib.nonNixTestedGhcVersions)
      config.services.haskell-ci.systems
      ## Add `include`d matrix entries to the required list.
      ++ map (
        entry:
          if entry.bounds == ""
          then "build (${entry.ghc}, ${entry.os})"
          else "build (${entry.bounds}, ${entry.ghc}, ${entry.os})"
      )
      config.services.haskell-ci.include);
  services.haskell-ci = {
    inherit (self.lib) defaultGhcVersion;
    ghcVersions = self.lib.nonNixTestedGhcVersions;
    cabalPackages = {"${config.project.name}" = "core";};
    ## The latest Stackage LTS that we also build on GitHub for.
    latestGhcVersion = "9.10.1";
  };

  ## publishing
  services.github.settings.repository.topics = ["recursion" "plugin"];
}
