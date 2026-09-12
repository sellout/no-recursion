{
  description = "A GHC plugin to remove support for recursion";

  nixConfig = {
    ## NB: This is a consequence both of the prevailing Haskell infrastructure
    ##     and of using `self.pkgsLib.runEmptyCommand`, which allows us to
    ##     sandbox derivations that otherwise can’t be. Even once we migrate to
    ##     non-IFD Haskell infra, this will probably still need to be enabled
    ##     for the other reason.
    allow-import-from-derivation = true;
    extra-substituters = ["https://sellout.cachix.org"];
    extra-trusted-public-keys = [
      "sellout.cachix.org-1:v37cTpWBEycnYxSPAgSQ57Wiqd3wjljni2aC0Xry1DE="
    ];
    ## WAIT: This should be `"fatal"`, but NixOS/nixpkgs#544986.
    lint-absolute-path-literals = "warn";
    lint-short-path-literals = "fatal";
    lint-url-literals = "fatal";
    ## Isolate the build.
    sandbox = "relaxed";
    use-registries = false;
  };

  ## The flake isn’t a Nix expression, so it’s clearer to keep `outputs` (which
  ## is) in a separate file.
  outputs = inputs: import .config/flake/outputs.nix inputs;

  inputs = {
    ## Flaky should generally be the source of truth for its inputs.
    flaky = {
      inputs.systems.follows = "systems";
      url = "github:sellout/flaky";
    };

    flake-utils.follows = "flaky/flake-utils";
    nixpkgs.follows = "flaky/nixpkgs";

    flaky-haskell = {
      inputs.flaky.follows = "flaky";
      url = "github:sellout/flaky-haskell";
    };

    ## Don’t inherit from Flaky, because we don’t support i686-linux.
    systems.url = "github:nix-systems/default";
  };
}
