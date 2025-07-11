{
  description = "Primer is a pedagogical functional programming language.";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";

    # We use this for some convenience functions only.
    hacknix.url = "github:hackworthltd/hacknix";

    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;

    pre-commit-hooks-nix.url = "github:cachix/git-hooks.nix";

    flake-parts.url = "github:hercules-ci/flake-parts";

    treefmt-nix.url = "github:numtide/treefmt-nix";

    # Let haskell.nix dictate the nixpkgs we use, as that will ensure
    # better haskell.nix cache hits.
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    hacknix.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks-nix.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    ghc-wasm.url = "git+https://gitlab.haskell.org/ghc/ghc-wasm-meta";
  };

  outputs = inputs@ { flake-parts, ... }:
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
          v = inputs.self.rev or inputs.self.lastModifiedDate;
        in
        builtins.trace "Nix Primer version is ${v}" "git-${v}";

      ghcVersion = "ghc9122";

      # We must keep the weeder version in sync with the version of
      # GHC we're using.
      weederVersion = "2.8.0";

      # Fourmolu updates often alter formatting arbitrarily, and we want to
      # have more control over this.
      fourmoluVersion = "0.18.0.0";

      allOverlays = [
        inputs.haskell-nix.overlay
        inputs.self.overlays.default
      ];

      # cabal-fmt needs an override for GHC > 9.8.1.
      cabal-fmt-override = {
        version = "latest";
        cabalProject = ''
          packages: .
          allow-newer: cabal-fmt:base
        '';
      };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      debug = true;

      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.treefmt-nix.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" ];

      perSystem = { config, pkgs, system, ... }:
        let
          # haskell.nix does a lot of heavy lifiting for us and gives us a
          # flake for our Cabal project with the following attributes:
          # `checks`, `apps`, and `packages`.
          primerFlake = pkgs.primer.flake { };

          weeder =
            let
              weederTool = pkgs.haskell-nix.tool ghcVersion "weeder" weederVersion;
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


          # Filter out any file in this repo that doesn't affect a Cabal
          # build or Haskell-related check. (Note: this doesn't need to be
          # 100% accurate, it's just an optimization to cut down on
          # extraneous Nix builds.)
          onlyHaskellSrc =
            let
              inherit (pkgs.haskell-nix) haskellSourceFilter;
              inherit (pkgs.haskell-nix.haskellLib) cleanGit cleanSourceWith;

              primerSourceFilter = name: type:
                let baseName = baseNameOf (toString name);
                in ! (
                  baseName == ".buildkite" ||
                  baseName == ".github" ||
                  baseName == "CODE_OF_CONDUCT.md" ||
                  baseName == "CONTRIBUTING.md" ||
                  baseName == "DCO.md" ||
                  baseName == "Makefile" ||
                  baseName == "README.md" ||
                  baseName == "SECURITY.md" ||
                  baseName == "bugreport.sh" ||
                  pkgs.lib.hasPrefix "cabal.project.local" baseName ||
                  baseName == "ci.nix" ||
                  baseName == "default.nix" ||
                  baseName == "docs" ||
                  baseName == "flake-compat.nix" ||
                  baseName == "flake.lock" ||
                  baseName == "flake.nix" ||
                  baseName == "nix" ||
                  baseName == "nixos-tests" ||
                  baseName == "shell.nix"
                );
            in
            cleanSourceWith {
              filter = haskellSourceFilter;
              name = "primer-src";
              src = cleanSourceWith
                {
                  filter = primerSourceFilter;
                  src = cleanGit
                    {
                      src = ./.;
                    };
                };
            };
        in
        {
          # We need a `pkgs` that includes our own overlays within
          # `perSystem`. This isn't done by default, so we do this
          # workaround. See:
          #
          # https://github.com/hercules-ci/flake-parts/issues/106#issuecomment-1399041045
          _module.args.pkgs = import inputs.nixpkgs
            {
              inherit system;
              config = {
                allowUnfree = true;
                allowBroken = true;
              };
              overlays = allOverlays;
            };

          pre-commit = {
            check.enable = true;
            settings = {
              src = ./.;
              hooks = {
                treefmt.enable = true;
                actionlint.enable = true;
              };
            };
          };

          packages = {
            inherit (pkgs) primer-benchmark;
          }
          // (pkgs.lib.optionalAttrs (system == "x86_64-linux") {
            inherit (pkgs) primer-benchmark-results-json;
            inherit (pkgs) primer-criterion-results-github-action-benchmark;
            inherit (pkgs) primer-benchmark-results-github-action-benchmark;
          })
          // primerFlake.packages;

          checks = {
            # Disabled, as it doesn't currently build with Nix.
            #inherit weeder;
          }

          # Temporarily disabled due to a failure in nixpkgs.
          # See:
          # https://github.com/hackworthltd/primer/issues/1114

          # // (pkgs.lib.optionalAttrs (system == "x86_64-linux")
          #   (inputs.hacknix.lib.testing.nixos.importFromDirectory ./nixos-tests
          #     {
          #       hostPkgs = pkgs;
          #       defaults.imports = [ inputs.self.nixosModules.default ];
          #     }
          #   )
          # )

          # Broken on NixOS. See:
          # https://github.com/hackworthltd/primer/issues/632
          // (pkgs.lib.optionalAttrs (system == "aarch64-darwin") {

            # We're using `source-repository-package`, so we must
            # disable this. See:
            # https://github.com/hackworthltd/primer/issues/876

            # Make sure HLS can typecheck our project.
            # check-hls = pkgs.callPackage ./nix/pkgs/check-hls {
            #   src = onlyHaskellSrc;

            #   # Don't use the flake's version here; we only want to run
            #   # this HLS check when the Haskell source files have
            #   # changed, not on every commit to this repo.
            #   version = "1.0";

            #   # This is a bit of a hack, but we don't know a better way.
            #   inherit (primerFlake) devShell;
            # };
          })
          // primerFlake.checks;

          apps =
            let
              mkApp = pkg: script: {
                type = "app";
                program = "${pkg}/bin/${script}";
              };
            in
            (pkgs.lib.mapAttrs (name: pkg: mkApp pkg name) {
              inherit (pkgs) primer-benchmark;
            })
            // primerFlake.apps;

          treefmt.config =
            let
              haskellExcludes = [
                "primer/test/outputs"
                "primer-api/test/outputs"
              ];

              haskellNixTools = pkgs.haskell-nix.tools ghcVersion {
                fourmolu = fourmoluVersion;
              };
            in
            {
              projectRootFile = "flake.nix";

              programs.hlint = {
                enable = true;
                package = pkgs.hlint;
              };
              programs.cabal-fmt = {
                enable = true;
                package = pkgs.cabal-fmt;
              };
              programs.fourmolu = {
                enable = true;
                package = haskellNixTools.fourmolu;
              };
              programs.nixpkgs-fmt.enable = true;
              programs.shellcheck.enable = true;

              settings.on-unmatched = "info";
              settings.formatter.hlint.excludes = haskellExcludes;
              settings.formatter.fourmolu.excludes = haskellExcludes;
            };

          devShells = {
            default = primerFlake.devShell // {
              inputsFrom = [
                config.treefmt.build.devShell
              ];
            };
            wasm = pkgs.mkShell {
              packages = with inputs.ghc-wasm.packages.${system};
                [
                  all_9_12

                  pkgs.gnumake
                  pkgs.simple-http-server
                  pkgs.brotli

                  # We need to run native `tasty-discover` at compile
                  # time, because we can't do it via `wasmtime`.
                  (pkgs.haskell-nix.tool ghcVersion "tasty-discover" { })
                ];
            };
          };
        };

      flake =
        let
          # See above, we need to use our own `pkgs` within the flake.
          pkgs = import inputs.nixpkgs
            {
              system = "x86_64-linux";
              config = {
                allowUnfree = true;
                allowBroken = true;
              };
              overlays = allOverlays;
            };
        in
        {
          overlays.default = (final: prev:
            let
              ghc982Tools = final.haskell-nix.tools "ghc982" {
                hlint = "latest";
                cabal-fmt = "latest";
                ghcid = "latest";
              };

              primer = final.haskell-nix.cabalProject {
                compiler-nix-name = ghcVersion;
                src = ./.;
                modules = [
                  {
                    # We want -Werror for Nix builds (primarily for CI).
                    packages =
                      let
                        # Tell Tasty to detect missing golden tests,
                        # rather than silently ignoring them.
                        #
                        # Until upstream addresses the issue, this is a
                        # workaround for
                        # https://github.com/hackworthltd/primer/issues/298
                        preCheckTasty = ''
                          export TASTY_NO_CREATE=true
                        '';
                      in
                      {
                        primer = {
                          ghcOptions = [ "-Werror" ];
                          preCheck = preCheckTasty;
                        };
                        primer-api = {
                          ghcOptions = [ "-Werror" ];
                          preCheck = preCheckTasty;
                        };
                        primer-benchmark = {
                          ghcOptions = [ "-Werror" ];
                          preCheck = preCheckTasty;
                        };
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
                    # Some packages are not visible to haskell.nix's planner, and need
                    # to be added manually.
                    #
                    # Ref:
                    # https://github.com/input-output-hk/haskell.nix/commit/61fbe408c01b6d61d010e6fb8e78bd19b5b025cc
                    package-keys = [
                      "bytestring-builder"
                      "diagrams"
                      "fail"
                    ];

                    # These packages don't/can't generate HIE files. See:
                    # https://github.com/input-output-hk/haskell.nix/issues/1242
                    packages.mtl-compat.writeHieFiles = false;
                    packages.bytestring-builder.writeHieFiles = false;
                    packages.fail.writeHieFiles = false;
                    packages.diagrams.writeHieFiles = false;
                    packages.happy-lib.writeHieFiles = false;
                  }
                  {
                    #TODO This shouldn't be necessary - see the commented-out `build-tool-depends` in primer.cabal.
                    packages.primer.components.tests.primer-test.build-tools = [ (final.haskell-nix.tool ghcVersion "tasty-discover" { }) ];
                    packages.primer-api.components.tests.primer-api-test.build-tools = [ (final.haskell-nix.tool ghcVersion "tasty-discover" { }) ];
                  }
                  (
                    let
                      # This makes it a lot easier to see which test is the culprit when CI fails.
                      hide-successes = [ "--hide-successes" ];
                      #TODO Haskell.nix would ideally pick this up from `cabal.project`.
                      # See: https://github.com/input-output-hk/haskell.nix/issues/1149#issuecomment-946664684
                      size-cutoff = [ "--size-cutoff=32768" ];
                    in
                    {
                      packages.primer.components.tests.primer-test.testFlags = hide-successes ++ size-cutoff;
                      packages.primer-api.components.tests.primer-api-test.testFlags = hide-successes ++ size-cutoff;
                      packages.primer-benchmark.components.tests.primer-benchmark-test.testFlags = hide-successes;
                    }
                  )
                ];

                shell = {
                  # We're using a `source-repository-package`, so we must disable this.
                  # See:
                  # https://github.com/hackworthltd/primer/issues/876
                  #exactDeps = true;
                  withHoogle = true;

                  tools = {
                    haskell-language-server = {
                      src = pkgs.haskell-nix.sources."hls-2.10";
                      cabalProjectLocal = ''
                        allow-newer: haddock-library:base
                      '';
                    };

                    implicit-hie = "latest";

                    cabal = "latest";

                    # Disabled, as it doesn't currently build with Nix.
                    #weeder = weederVersion;

                    fourmolu = fourmoluVersion;

                    #TODO Explicitly requiring tasty-discover shouldn't be necessary - see the commented-out `build-tool-depends` in primer.cabal.
                    tasty-discover = "latest";
                  };

                  buildInputs = (with final; [
                    nixpkgs-fmt

                    hlint
                    cabal-fmt
                    ghcid

                    # For Language Server support.
                    nodejs_22
                  ]);

                  shellHook = ''
                    export HIE_HOOGLE_DATABASE="$(cat $(${final.which}/bin/which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
                  '';
                };
              };

              primerFlake = primer.flake { };

              # Note: these benchmarks should only be run (in CI) on a
              # "benchmark" machine. This is enforced for our CI system
              # via Nix's `requiredSystemFeatures`.
              #
              # The `lastEnvChange` value is an impurity that we can
              # modify when we want to force a new benchmark run
              # despite the benchmarking code not having changed, as
              # otherwise Nix will cache the results. It's intended to
              # be used to track changes to the benchmarking
              # environment, such as changes to hardware, that Nix
              # doesn't know about.
              #
              # The value should be formatted as an ISO date, followed
              # by a "." and a 2-digit monotonic counter, to allow for
              # multiple changes on the same date. We store this value
              # in a `lastEnvChange` file in the derivation output, so
              # that we can examine results in the Nix store and know
              # which benchmarking environment was used to generate
              # them.
              benchmarks =
                let
                  lastEnvChange = "20240408.02";
                in
                final.callPackage ./nix/pkgs/benchmarks {
                  inherit lastEnvChange;
                };
            in
            {
              lib = (prev.lib or { }) // {
                primer = (prev.lib.primer or { }) // {
                  inherit version;
                };
              };

              inherit primer;

              primer-benchmark = primerFlake.packages."primer-benchmark:bench:primer-benchmark";

              inherit (benchmarks) primer-benchmark-results-json;
              inherit (benchmarks) primer-criterion-results-github-action-benchmark;
              inherit (benchmarks) primer-benchmark-results-github-action-benchmark;

              inherit (ghc982Tools) cabal-fmt hlint ghcid;
            }
          );

          nixosModules.default = {
            nixpkgs.overlays = allOverlays;
          };

          hydraJobs = {
            inherit (inputs.self) packages;
            inherit (inputs.self) checks;
            inherit (inputs.self) devShells;

            required-ci = pkgs.releaseTools.aggregate {
              name = "required-ci";
              constituents = builtins.map builtins.attrValues (with inputs.self.hydraJobs; [
                packages.x86_64-linux
                packages.aarch64-darwin
                checks.x86_64-linux
                checks.aarch64-darwin
              ]);
              meta.description = "Required CI builds";
            };
          };

          ciJobs = inputs.hacknix.lib.flakes.recurseIntoHydraJobs inputs.self.hydraJobs;
        };
    };
}
