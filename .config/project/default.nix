{
  config,
  flaky,
  lib,
  pkgs,
  self,
  supportedSystems,
  ...
}: let
  githubSystems = [
    "macos-13" # x86_64-darwin
    "macos-14" # aarch64-darwin
    "ubuntu-22.04" # x86_64-linux
    "windows-2022"
  ];
in {
  project = {
    name = "no-recursion";
    summary = "A GHC plugin to remove support for recursion";

    devPackages = [
      pkgs.cabal-install
      pkgs.graphviz
      ## So cabal-plan(-bounds) can be built in a devShell, since it doesn’t
      ## work in Nix proper.
      pkgs.zlib
    ];
  };

  imports = [
    (import ./github-ci.nix {
      inherit (self.lib) defaultGhcVersion;
      exclude =
        [
          ## These fail to build Cabal-syntax – since it works on most
          ## OSes with `--prefer-oldest`, it may be a dependency version
          ## issue.
          {
            bounds = "--prefer-oldest";
            ghc = "8.4.1";
            os = "windows-2022";
          }
          {
            bounds = "";
            ghc = "8.4.1";
          }
          ## Plugins are broken on Windows from GHC 8.6.1 to 8.6.4.
          ## (See https://gitlab.haskell.org/ghc/ghc/-/issues/15700)
          {
            ghc = "8.6.1";
            os = "windows-2022";
          }
        ]
        ## These fail to build hsc2hs, perhaps related to
        ## https://stackoverflow.com/questions/32740172/unresolved-stdio-common-vsprintf-s-what-library-has-this.
        ++ map (ghc: {
          inherit ghc;
          os = "windows-2022";
        }) ["7.10.3" "8.0.2" "8.2.2"]
        ## TODO: Broken or flaky builds that need to be analyzed
        ++ map (ghc: {
          inherit ghc;
          os = "windows-2022";
        }) ["8.8.1" "8.10.1"]
        ++ [
          {
            ghc = "9.4.1";
            os = "macos-14";
          }
        ];
      ## These replace the some of the builds excluded above..
      include = let
        bounds = ["--prefer-oldest" ""];
      in
        ## TODO: Figure out what’s going on here.
        # map (bounds: {
        #   inherit bounds;
        #   ghc = "7.10.3";
        #   os = "ubuntu-20.04";
        # })
        # bounds
        [
          {
            bounds = "--prefer-oldest";
            ghc = "8.4.4"; # TODO: Might work on 8.4.2–8.4.3
            os = "windows-2022";
          }
        ]
        ++ map (os: {
          inherit os;
          bounds = "";
          ghc = "8.4.4"; # TODO: Might work on 8.4.2–8.4.3
        })
        ## No aarch64-darwin support in GHC 8.4
        (lib.remove "macos-14" githubSystems)
        ++ map (bounds: {
          inherit bounds;
          ghc = "8.6.5";
          os = "windows-2022";
        })
        bounds
        ++ lib.concatMap (bounds:
          map (ghc: {
            inherit bounds ghc;
            os = "windows-2022";
          }) ["8.10.7"]) # TODO: Might work on .2–.6
        
        bounds;
      latestGhcVersion = "9.10.1";
      packages = {"${config.project.name}" = config.project.name;};
      systems = githubSystems;
    })
    ./hackage-publish.nix
    ./hlint.nix
  ];

  ## dependency management
  services.renovate.enable = true;

  ## development
  programs = {
    direnv.enable = true;
    # This should default by whether there is a .git file/dir (and whether it’s
    # a file (worktree) or dir determines other things – like where hooks
    # are installed.
    git = {
      enable = true;
      ignores = [
        # Cabal build
        "dist-newstyle"
      ];
    };
  };

  ## formatting
  editorconfig.enable = true;

  programs = {
    treefmt = {
      enable = true;
      ## Haskell formatter
      programs.ormolu.enable = true;
      settings.formatter.prettier.excludes = ["*/docs/license-report.md"];
    };
    vale = {
      enable = true;
      excludes = [
        "*.cabal"
        "*.hs"
        "*.lhs"
        "*/docs/license-report.md"
        "./cabal.project"
      ];
      vocab.${config.project.name}.accept = [
        "API"
        "bugfix"
        "comonad"
        "conditionalize"
        "formatter"
        "functor"
        "GADT"
        "inline"
        "Kleisli"
        "Kmett"
        "pragma"
        "unformatted"
        "widening"
      ];
    };
  };

  ## CI
  services.garnix = {
    enable = true;
    builds = {
      ## TODO: Remove once garnix-io/garnix#285 is fixed.
      exclude = ["homeConfigurations.x86_64-darwin-example"];
      include = lib.mkForce (
        [
          "homeConfigurations.*"
          "nixosConfigurations.*"
        ]
        ++ flaky.lib.forGarnixSystems supportedSystems (
          sys:
            [
              "checks.${sys}.*"
              "devShells.${sys}.default"
              "packages.${sys}.default"
            ]
            ++ lib.concatMap (version: let
              ghc = self.lib.nixifyGhcVersion version;
            in [
              "devShells.${sys}.${ghc}"
              "packages.${sys}.${ghc}_all"
            ])
            (self.lib.testedGhcVersions sys)
        )
      );
    };
  };
  ## FIXME: Shouldn’t need `mkForce` here (or to duplicate the base contexts).
  ##        Need to improve module merging.
  services.github.settings.branches.main.protection.required_status_checks.contexts =
    lib.mkForce
    (["check-bounds"]
      ++ lib.concatMap (sys:
        lib.concatMap (ghc: [
          "build (${ghc}, ${sys})"
          "build (--prefer-oldest, ${ghc}, ${sys})"
        ])
        self.lib.nonNixTestedGhcVersions)
      githubSystems
      ++ flaky.lib.forGarnixSystems supportedSystems (sys:
        lib.concatMap (version: let
          ghc = self.lib.nixifyGhcVersion version;
        in [
          "devShell ${ghc} [${sys}]"
          "package ${ghc}_all [${sys}]"
        ])
        (self.lib.testedGhcVersions sys)
        ++ [
          "homeConfig ${sys}-${config.project.name}-example"
          "package default [${sys}]"
          ## FIXME: These are duplicated from the base config
          "check formatter [${sys}]"
          "check project-manager-files [${sys}]"
          "check vale [${sys}]"
          "devShell default [${sys}]"
        ]));

  ## publishing
  # NB: Can’t use IFD on FlakeHub (see DeterminateSystems/flakehub-push#69), so
  #     this is disabled until we have a way to build Haskell without IFD.
  services.flakehub.enable = false;
  services.github.enable = true;
  services.github.settings.repository.topics = ["recursion" "plugin"];
}
