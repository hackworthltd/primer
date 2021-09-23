{
  description = "Primer is a pedagogical functional programming language.";

  inputs = {
    haskell-nix.url = github:input-output-hk/haskell.nix;

    # Let haskell.nix dictate the nixpkgs we use, as that will ensure
    # better haskell.nix cache hits.
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    hacknix.url = github:hackworthltd/hacknix;
    flake-utils.url = github:numtide/flake-utils;

    flake-compat.url = github:edolstra/flake-compat;
    flake-compat.flake = false;

    pre-commit-hooks-nix.url = github:cachix/pre-commit-hooks.nix;
    pre-commit-hooks-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self
    , nixpkgs
    , haskell-nix
    , hacknix
    , flake-utils
    , pre-commit-hooks-nix
    , ...
    }@inputs:
    let
      # A flake can get its git revision via `self.rev` if its working
      # tree is clean and its index is empty, so we use that for the
      # program version when it's available.
      #
      # When the working tree is modified or the index is not empty,
      # evaluating `self.rev` is an error. However, we *can* use
      # `self.lastModifiedDate` in that case, which is at least a bit
      # more helpful than returning "unknown" or some other static
      # value. (This should only happen when you `nix run` in a
      # modified repo. Hydra builds will always be working from a
      # clean git repo, of course.)
      version =
        let
          v = self.rev or self.lastModifiedDate;
        in
        builtins.trace "Nix Primer version is ${v}" v;

      ghcVersion = "ghc8107";

      forAllSupportedSystems = flake-utils.lib.eachSystem [
        "x86_64-linux"
        "x86_64-darwin"
      ];

      forAllTestSystems = flake-utils.lib.eachSystem [
        "x86_64-linux"
      ];

      overlay = hacknix.lib.overlays.combine [
        haskell-nix.overlay
        hacknix.overlay
        (final: prev:
          let
            primer = final.haskell-nix.cabalProject {
              compiler-nix-name = ghcVersion;
              src = ./.;
              modules = [
                {
                  # We want -Werror for Nix builds (primarily for CI).
                  packages = {
                    primer.ghcOptions = [ "-Werror" ];
                    primer-selda.ghcOptions = [ "-Werror" ];
                    primer-service.ghcOptions = [ "-Werror" ];
                  };
                }
                {
                  # Build everything with -O2.
                  configureFlags = [ "-O2" ];

                  # Generate HIE files for everything.
                  writeHieFiles = true;

                  # Generate nice Haddocks & a Hoogle index for
                  # everything.
                  doHaddock = true;
                  doHyperlinkSource = true;
                  doQuickjump = true;
                  doHoogle = true;
                }
                {
                  #TODO This shouldn't be necessary - see the commented-out `build-tool-depends` in primer.cabal.
                  packages.primer.components.tests.primer-test.build-tools = [ final.haskell-nix.haskellPackages.tasty-discover ];
                }
              ];
            };
            primerFlake = primer.flake { };

            ghcjsPrimer = final.haskell-nix.cabalProject {
              cabalProjectFileName = "cabal.ghcjs.project";
              compiler-nix-name = ghcVersion;
              src = ./.;
              modules = [
                {
                  # We want -Werror for Nix builds (primarily for CI).
                  packages = {
                    primer.ghcOptions = [ "-Werror" ];
                  };
                }
                {
                  # Build everything with -O2.
                  configureFlags = [ "-O2" ];
                }
              ];
            };
            ghcjsPrimerFlake = ghcjsPrimer.flake {
              crossPlatforms = p: [ p.ghcjs ];
            };

            # Ensure these scripts get built for all supported
            # platforms by overriding their `meta.platforms`.
            # Otherwise, they'll only be built for Linux.

            run-primer = (final.writeShellScriptBin "run-primer"
              "${final.primer-service}/bin/primer-service serve . ${version} $@").overrideAttrs (drv: {
              meta.platforms = final.lib.platforms.all;
            });

            run-primer-local-pgsql = (final.writeShellScriptBin "run-primer-local-pgsql"
              "${final.primer-service}/bin/primer-service serve . ${version} --pgsql-url postgres://postgres:foobar@localhost:5432/primer $@").overrideAttrs (drv: {
              meta.platforms = final.lib.platforms.all;
            });

            create-local-pgsql-db = (final.writeShellScriptBin "create-local-pgsql-db"
              "${final.postgresql}/bin/createdb -h localhost -U postgres primer $@").overrideAttrs
              (drv: {
                meta.platforms = final.lib.platforms.all;
              });
          in
          {
            inherit primer;
            inherit ghcjsPrimer;

            primer-service = primerFlake.packages."primer-service:exe:primer-service";

            inherit run-primer run-primer-local-pgsql create-local-pgsql-db;
          }
        )
      ];

      pkgsFor = system: import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
          allowBroken = true;
        };
        overlays = [
          overlay
        ];
      };
    in
    {
      # Note: `overlay` is not per-system like most other flake attributes.
      inherit overlay;
    }

    // forAllSupportedSystems (system:
    let
      pkgs = pkgsFor system;

      # haskell.nix does a lot of heavy lifiting for us and gives us a
      # flake for our Cabal project with the following attributes:
      # `checks`, `apps`, and `packages`.
      #
      # When merging package sets, make sure to put the
      # ghcjsPrimerFlake first, so that the primerFlake will
      # override any commonly-named attributes. We only want the ghcjs
      # parts of the `ghcjsPrimerFlake` flake.

      ghcjsPrimerFlake = pkgs.ghcjsPrimer.flake {
        crossPlatforms = p: [ p.ghcjs ];
      };

      primerFlake = pkgs.primer.flake { };

      weeder =
        let
          weederTool = pkgs.haskell-nix.tool ghcVersion "weeder" "latest";
          getLibHIE = package:
            pkgs.lib.optional (package.components ? library)
              { name = "${package.identifier.name}-library"; path = package.components.library.hie; };
          getHIE = package: component: pkgs.lib.lists.map
            (cn: {
              name = "${package.identifier.name}-${component}-${cn}";
              path = package.components.${component}.${cn}.hie;
            })
            (builtins.attrNames package.components.${component});
          getHIEs = package:
            getLibHIE package
            ++ pkgs.lib.concatMap (getHIE package)
              [ "benchmarks" "exes" "sublibs" "tests" ];
          primer-packages = pkgs.haskell-nix.haskellLib.selectProjectPackages pkgs.primer;
        in
        pkgs.runCommand "weeder"
          {
            weederConfig = ./weeder.dhall;
            allHieFiles = pkgs.linkFarm
              "primer-hie-files"
              (pkgs.lib.concatMap getHIEs (builtins.attrValues primer-packages));
          }
          ''
            export XDG_CACHE_HOME=$(mktemp -d)
            ${weederTool}/bin/weeder --config $weederConfig --hie-directory $allHieFiles
            echo "No issues found."
            touch $out
          '';

      pre-commit-hooks =
        let
          # Override the default nix-pre-commit-hooks tools with the version
          # we're using.
          haskellNixTools = pkgs.haskell-nix.tools ghcVersion {
            hlint = "latest";
            fourmolu = "latest";
            cabal-fmt = "latest";
          };
        in
        pre-commit-hooks-nix.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            fourmolu.enable = true;
            cabal-fmt.enable = true;
            nixpkgs-fmt.enable = true;
          };

          # Override the default nix-pre-commit-hooks tools with the version
          # we're using.
          tools = {
            inherit (pkgs) nixpkgs-fmt;
          } // haskellNixTools;

          excludes = [ ];
        };
    in
    {
      packages =
        (hacknix.lib.flakes.filterPackagesByPlatform system
          ({
            inherit (pkgs) primer-service;
            inherit (pkgs) run-primer run-primer-local-pgsql create-local-pgsql-db;
          })
        )
        // ghcjsPrimerFlake.packages
        // primerFlake.packages;

      # Notes:
      #
      # - Don't include the `ghcjsPrimerFlake` checks, as they don't
      #   actually work since they won't be run in a browser.
      checks =
        {
          source-code-checks = pre-commit-hooks;
          weeder = weeder;
        }
        // primerFlake.checks;

      apps = {
        inherit (pkgs) run-primer run-primer-local-pgsql create-local-pgsql-db;
      }
      // ghcjsPrimerFlake.apps
      // primerFlake.apps;

      defaultApp = self.apps.${system}.run-primer;

      devShell = pkgs.primer.shellFor {
        tools = {
          ghcid = "latest";
          haskell-language-server = "latest";
          cabal = "latest";
          hlint = "latest";
          fourmolu = "latest";
          cabal-edit = "latest";
          cabal-fmt = "latest";
          #TODO Explicitly requiring tasty-discover shouldn't be necessary - see the commented-out `build-tool-depends` in primer.cabal.
          tasty-discover = "latest";
          weeder = "latest";
        };

        buildInputs = (with pkgs; [
          nixpkgs-fmt
          sqlite
          postgresql
        ]) ++ [
          (
            pkgs.haskell.lib.justStaticExecutables
              pkgs.haskellPackages.structured-haskell-mode
          )
        ];

        shellHook = ''
          export HIE_HOOGLE_DATABASE="$(cat $(${pkgs.which}/bin/which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
        '';

        # Make this buildable on Hydra.
        meta.platforms = pkgs.lib.platforms.unix;
      };
    })

    // {
      hydraJobs = {
        inherit (self) packages;
        inherit (self) checks;
        inherit (self) devShell;

        required =
          let
            pkgs = pkgsFor "x86_64-linux";
          in
          pkgs.releaseTools.aggregate {
            name = "required";
            constituents = builtins.map builtins.attrValues (with self.hydraJobs; [
              packages.x86_64-linux
              packages.x86_64-darwin
              checks.x86_64-linux
              checks.x86_64-darwin
              devShell
            ]);
            meta.description = "Required CI builds";
          };
      };

      ciJobs = hacknix.lib.flakes.recurseIntoHydraJobs self.hydraJobs;
    };
}
