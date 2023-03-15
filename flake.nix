{
  description = "Primer is a pedagogical functional programming language.";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";

    # We use this for some convenience functions only.
    hacknix.url = "github:hackworthltd/hacknix";

    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;

    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";

    flake-parts.url = "github:hercules-ci/flake-parts";

    # Let haskell.nix dictate the nixpkgs we use, as that will ensure
    # better haskell.nix cache hits.
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    hacknix.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks-nix.inputs.nixpkgs.follows = "nixpkgs";
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

      ghcVersion = "ghc961";

      # We must keep the weeder version in sync with the version of
      # GHC we're using.
      weederVersion = "2.4.0";

      # Fourmolu updates often alter formatting arbitrarily, and we want to
      # have more control over this.
      fourmoluVersion = "0.10.1.0";

      allOverlays = [
        inputs.haskell-nix.overlay
        inputs.self.overlays.default
      ];
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      debug = true;

      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        ./nix/flake-parts/benchmarks.nix
      ];
      systems = [ "x86_64-linux" ];

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

          openapi-validate = pkgs.runCommand "openapi-validate" { }
            ''
              ${pkgs.openapi-generator-cli}/bin/openapi-generator-cli validate --recommend -i ${pkgs.primer-openapi-spec}
              echo "No issues found."
              touch $out
            '';

          # This should go in `primer-sqitch.passthru.tests`, but
          # those don't work well with flakes.
          #
          # Note that the equivalent PostgreSQL tests need some tear
          # up/tear down, so we test that backend using NixOS tests.
          primer-sqitch-test-sqlite = pkgs.runCommand "primer-sqitch-sqlite-test" { } ''
            ${pkgs.primer-sqitch}/bin/primer-sqitch deploy --verify db:sqlite:primer.db
            ${pkgs.primer-sqitch}/bin/primer-sqitch revert db:sqlite:primer.db
            ${pkgs.primer-sqitch}/bin/primer-sqitch verify db:sqlite:primer.db | grep "No changes deployed"
            ${pkgs.primer-sqitch}/bin/primer-sqitch deploy --verify db:sqlite:primer.db
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
                  pkgs.lib.hasPrefix "cabal.project.local" baseName ||
                  baseName == "flake.lock" ||
                  baseName == "flake.nix" ||
                  baseName == "README.md" ||
                  baseName == "docs" ||
                  baseName == "nix" ||
                  baseName == "nixos-tests" ||
                  baseName == "sqitch"
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

          pre-commit =
            let
              # Override the default nix-pre-commit-hooks tools with the version
              # we're using.
              haskellNixTools = pkgs.haskell-nix.tools ghcVersion { };
            in
            {
              check.enable = true;
              settings = {
                src = ./.;
                hooks = {
                  nixpkgs-fmt.enable = true;

                  actionlint = {
                    enable = true;
                    name = "actionlint";
                    entry = "${pkgs.actionlint}/bin/actionlint";
                    language = "system";
                    files = "^.github/workflows/";
                  };
                };

                # We need to force these due to
                #
                # https://github.com/cachix/pre-commit-hooks.nix/issues/204
                tools = {
                  nixpkgs-fmt = pkgs.lib.mkForce pkgs.nixpkgs-fmt;
                };

                excludes = [
                  "primer/test/outputs"
                  "primer-service/test/outputs"
                  ".buildkite/"
                ];
              };
            };

          packages = {
            inherit (pkgs) primer-service primer-openapi-spec;
            inherit (pkgs) primer-benchmark;
            inherit (pkgs)
              run-primer-postgresql
              run-primer-sqlite
              primer-service-entrypoint

              create-local-db
              deploy-local-db
              verify-local-db
              revert-local-db
              status-local-db
              log-local-db
              delete-local-db
              dump-local-db
              restore-local-db
              connect-local-db
              delete-all-local-sessions

              sqitch
              primer-sqitch
              primer-pg-prove

              deploy-postgresql-container
              start-postgresql-container
              stop-postgresql-container;
          }
          // (pkgs.lib.optionalAttrs (system == "x86_64-linux") {
            inherit (pkgs) primer-service-docker-image;
          })
          // primerFlake.packages;

          checks = {
            inherit weeder openapi-validate;

            inherit primer-sqitch-test-sqlite;
          }
          // (pkgs.lib.optionalAttrs (system == "x86_64-linux")
            (inputs.hacknix.lib.testing.nixos.importFromDirectory ./nixos-tests
              {
                hostPkgs = pkgs;
                defaults.imports = [ inputs.self.nixosModules.default ];
              }
            )
          )

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
              inherit (pkgs) primer-openapi-spec;
              inherit (pkgs) primer-benchmark;

              inherit (pkgs)
                run-primer-postgresql
                run-primer-sqlite
                primer-service-entrypoint

                create-local-db
                deploy-local-db
                verify-local-db
                revert-local-db
                status-local-db
                log-local-db
                delete-local-db
                dump-local-db
                restore-local-db
                connect-local-db
                delete-all-local-sessions

                primer-sqitch

                deploy-postgresql-container
                start-postgresql-container
                stop-postgresql-container;
            })
            // primerFlake.apps;

          devShells.default = primerFlake.devShell;

          # This is a non-standard flake output, but we don't want to
          # include benchmark runs in `packages`, because we don't
          # want them to be part of the `hydraJobs` or `ciJobs`
          # attrsets. The benchmarks need to be run in a more
          # controlled environment, and this gives us that
          # flexibility.
          benchmarks = {
            inherit (pkgs) primer-benchmark-results-html;
            inherit (pkgs) primer-benchmark-results-json;
            inherit (pkgs) primer-criterion-results-github-action-benchmark;
          }
          // (pkgs.lib.optionalAttrs (system == "x86_64-linux")
            (
              pkgs.nixos-bench // { inherit (pkgs) primer-benchmark-results-github-action-benchmark; }

            ));
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

          benchmarkJobs = {
            inherit (inputs.self.benchmarks) x86_64-linux;

            required-benchmarks = pkgs.releaseTools.aggregate {
              name = "required-benchmarks";
              constituents = builtins.map builtins.attrValues (with inputs.self; [
                benchmarks.x86_64-linux
              ]);
              meta.description = "Required CI benchmarks";
            };
          };
        in
        {
          overlays.default = (final: prev:
            let
              postgres-dev-password = "primer-dev";
              postgres-dev-base-url = "postgres://postgres:${postgres-dev-password}@localhost:5432";
              postgres-dev-primer-url = "${postgres-dev-base-url}/primer";

              sqitch = final.callPackage ./nix/pkgs/sqitch {
                postgresqlSupport = true;
                sqliteSupport = true;
              };

              scripts = final.lib.recurseIntoAttrs (final.callPackage ./nix/pkgs/scripts {
                sqitchDir = ./sqitch;
                inherit version;
              });

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
                        primer-rel8 = {
                          ghcOptions = [ "-Werror" ];
                          preCheck = preCheckTasty;
                        };
                        primer-service = {
                          ghcOptions = [ "-Werror" ];

                          # The tests need PostgreSQL binaries.
                          preCheck = ''
                            export PATH="${final.postgresql}/bin:${"$PATH"}"
                          '' + preCheckTasty;
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
                    # These packages don't generate HIE files. See:
                    # https://github.com/input-output-hk/haskell.nix/issues/1242
                    packages.mtl-compat.writeHieFiles = false;
                    packages.bytestring-builder.writeHieFiles = false;
                  }
                  {
                    #TODO This shouldn't be necessary - see the commented-out `build-tool-depends` in primer.cabal.
                    packages.primer.components.tests.primer-test.build-tools = [ (final.haskell-nix.tool ghcVersion "tasty-discover" { }) ];
                    packages.primer-rel8.components.tests.primer-rel8-test.build-tools = [
                      (final.haskell-nix.tool ghcVersion "tasty-discover" { })
                      final.postgresql
                      final.primer-sqitch
                    ];
                    packages.primer-selda.components.tests.primer-selda-test.build-tools = [
                      (final.haskell-nix.tool ghcVersion "tasty-discover" { })
                      final.primer-sqitch
                    ];
                    packages.primer-service.components.tests.service-test.build-tools = [
                      (final.haskell-nix.tool ghcVersion "tasty-discover" { })
                      final.postgresql
                      final.primer-sqitch
                    ];
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
                      packages.primer-service.components.tests.service-test.testFlags = hide-successes ++ size-cutoff;
                      packages.primer-rel8.components.tests.primer-rel8-test.testFlags = hide-successes;
                      packages.primer-selda.components.tests.primer-selda-test.testFlags = hide-successes;
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
                    ghcid = "latest";
                    haskell-language-server = "latest";
                    implicit-hie = "latest";

                    cabal = "latest";
                    weeder = weederVersion;

                    #TODO Explicitly requiring tasty-discover shouldn't be necessary - see the commented-out `build-tool-depends` in primer.cabal.
                    tasty-discover = "latest";
                  };

                  buildInputs = (with final; [
                    nixpkgs-fmt
                    postgresql
                    sqlite
                    openapi-generator-cli

                    # For Docker support.
                    docker
                    lima
                    colima

                    # For Language Server support.
                    nodejs-16_x

                    # sqitch & related
                    nix-generate-from-cpan
                    sqitch
                    primer-sqitch
                    primer-pg-prove

                    # Local scripts.
                    create-local-db
                    deploy-local-db
                    verify-local-db
                    revert-local-db
                    status-local-db
                    log-local-db
                    delete-local-db
                    dump-local-db
                    restore-local-db
                    connect-local-db
                    delete-all-local-sessions
                  ]);

                  shellHook = ''
                    gen-hie > hie.yaml
                    export HIE_HOOGLE_DATABASE="$(cat $(${final.which}/bin/which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
                  '';
                };
              };

              primerFlake = primer.flake { };

              # Generate the Primer service OpenAPI 3 spec file.
              primer-openapi-spec = (final.runCommand "primer-openapi" { }
                "${final.primer-openapi}/bin/primer-openapi > $out").overrideAttrs
                (drv: {
                  meta.platforms = final.lib.platforms.all;
                });

              primer-service-docker-image = final.dockerTools.buildLayeredImage {
                name = "primer-service";
                tag = version;
                contents = [
                  final.primer-service-entrypoint
                ]
                ++ (with final; [
                  # These are helpful for debugging broken images.
                  bashInteractive
                  coreutils
                  lsof
                  procps
                ]);

                config =
                  let port = final.lib.primer.defaultServicePort;
                  in
                  {
                    Entrypoint = [ "/bin/primer-service-entrypoint" ];

                    # Note that we can't set
                    # "org.opencontainers.image.created" here because
                    # it would introduce an impurity. If we want to
                    # set it, we'll need to set it when we push to a
                    # registry.
                    Labels = {
                      "org.opencontainers.image.source" =
                        "https://github.com/hackworthltd/primer";
                      "org.opencontainers.image.documentation" =
                        "https://github.com/hackworthltd/primer";
                      "org.opencontainers.image.title" = "primer-service";
                      "org.opencontainers.image.description" =
                        "The Primer API service.";
                      "org.opencontainers.image.version" = version;
                      "org.opencontainers.image.authors" =
                        "src@hackworthltd.com";
                      "org.opencontainers.image.licenses" = "AGPL-3.0";
                      "org.opencontainers.image.vendor" = "Hackworth Ltd";
                      "org.opencontainers.image.url" =
                        "https://github.com/hackworthltd/primer";
                      "org.opencontainers.image.revision" = inputs.self.rev or "dirty";
                    };

                    ExposedPorts = { "${toString port}/tcp" = { }; };

                    Env = [
                      # Environment variables required by the
                      # entrypoint with reasonable default values.
                      #
                      # Note that we do not provide default values or
                      # otherwise set either DATABASE_URL or
                      # SQLITE_DB.
                      "SERVICE_PORT=${toString port}"
                      "PRIMER_VERSION=${version}"

                      # Needed for the `primer-service` banner.
                      "LANG=C.UTF-8"

                      # Sqitch will fail in a container if these are not
                      # set. Their specific values are not important.
                      "SQITCH_EMAIL=root@localhost"
                      "SQITCH_FULLNAME=Primer User"
                    ];
                  };
              };

              # The version used in haskell.nix nixpkgs is broken, so we
              # override it until that's fixed.
              colima = final.callPackage ./nix/pkgs/colima { };


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
                  lastEnvChange = "20230130.01";
                in
                final.callPackage ./nix/pkgs/benchmarks {
                  inherit lastEnvChange;
                  inherit (inputs.hacknix.lib.testing.nixos) importFromDirectory;
                  inherit (inputs.self) nixosModules;
                };
            in
            {
              lib = (prev.lib or { }) // {
                primer = (prev.lib.primer or { }) // {
                  defaultServicePort = 8081;
                  inherit version;
                  inherit postgres-dev-password;
                  inherit postgres-dev-base-url;
                  inherit postgres-dev-primer-url;
                };
              };

              inherit sqitch;

              inherit (scripts)
                deploy-postgresql-container
                start-postgresql-container
                stop-postgresql-container
                create-local-db
                deploy-local-db
                verify-local-db
                revert-local-db
                status-local-db
                log-local-db
                delete-local-db
                dump-local-db
                restore-local-db
                primer-sqitch
                primer-pg-prove
                connect-local-db
                delete-all-local-sessions
                run-primer-postgresql
                run-primer-sqlite
                primer-service-entrypoint;

              inherit primer;

              primer-service = primerFlake.packages."primer-service:exe:primer-service";
              primer-openapi = primerFlake.packages."primer-service:exe:primer-openapi";
              primer-replay = primerFlake.packages."primer-service:exe:primer-replay";
              primer-benchmark = primerFlake.packages."primer-benchmark:bench:primer-benchmark";

              inherit primer-service-docker-image;

              inherit primer-openapi-spec;

              inherit colima;

              # Note to the reader: these derivations run benchmarks
              # and collect the results in various formats. They're
              # part of the flake's overlay, so they appear in any
              # `pkgs` that uses this overlay. Hoewver, we do *not*
              # include these in the flake's `packages` output,
              # because we don't want them to be built/run when CI
              # evaluates the `hydraJobs` or `ciJobs` outputs.
              inherit (benchmarks) primer-benchmark-results-html;
              inherit (benchmarks) primer-benchmark-results-json;
              inherit (benchmarks) primer-criterion-results-github-action-benchmark;
              inherit (benchmarks) nixos-bench;
              inherit (benchmarks) primer-benchmark-results-github-action-benchmark;
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
                checks.x86_64-linux
              ]);
              meta.description = "Required CI builds";
            };
          };

          ciJobs = inputs.hacknix.lib.flakes.recurseIntoHydraJobs inputs.self.hydraJobs;
          ciBenchmarks = inputs.hacknix.lib.flakes.recurseIntoHydraJobs benchmarkJobs;
        };
    };
}
