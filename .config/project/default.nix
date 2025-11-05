{
  config,
  flaky,
  lib,
  pkgs,
  self,
  supportedSystems,
  ...
}: {
  project = {
    name = "no-recursion";
    summary = "A GHC plugin to remove support for recursion";
    file = {
      "core/LICENSE".source = ../../LICENSE;
      "core/LICENSE.AGPL-3.0-only".source = ../../LICENSE.AGPL-3.0-only;
      "core/LICENSE.Universal-FOSS-exception-1.0".source =
        ../../LICENSE.Universal-FOSS-exception-1.0;
      "core/LICENSE.commercial".source = ../../LICENSE.commercial;
    };
  };

  imports = [./hlint.nix];

  ## dependency management
  services.renovate.enable = true;

  ## development
  programs = {
    direnv.enable = true;
    # This should default by whether there is a .git file/dir (and whether it’s
    # a file (worktree) or dir determines other things – like where hooks
    # are installed.
    git.enable = true;
  };

  programs = {
    treefmt.enable = true;
    vale.enable = true;
  };

  ## formatting
  editorconfig.enable = true;

  ## CI
  services.garnix.enable = true;
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
        lib.concatMap (ghc: [
          "build (${ghc}, ${sys})"
          "build (--prefer-oldest, ${ghc}, ${sys})"
        ])
        self.lib.nonNixTestedGhcVersions)
      config.services.haskell-ci.systems);
  services.haskell-ci = {
    inherit (self.lib) defaultGhcVersion;
    ghcVersions = self.lib.nonNixTestedGhcVersions;
    cabalPackages = {"${config.project.name}" = "core";};
    extraDependencyVersions = ["doctest-0.24.0"];
    latestGhcVersion = "9.10.1";
  };

  ## publishing
  services.github.enable = true;
  services.github.settings.repository.topics = ["recursion" "plugin"];
}
