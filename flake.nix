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
    # Fixes aarch64-darwin support.
    pre-commit-hooks-nix.inputs.flake-utils.follows = "flake-utils";

    # Temporary workaround for HLS issues until the next release.
    haskell-language-server.url = github:haskell/haskell-language-server;
    haskell-language-server.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , haskell-nix
    , hacknix
    , flake-utils
    , pre-commit-hooks-nix
    , haskell-language-server
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
        builtins.trace "Nix Primer version is ${v}" "git-${v}";

      # Workaround for https://github.com/input-output-hk/haskell.nix/issues/1177.
      exceptionsWorkaround = version: {
        inherit version;
        modules = [
          ({ lib, ... }: {
            reinstallableLibGhc = true;
          })
        ];
      };

      # https://github.com/input-output-hk/haskell.nix/issues/1177
      nonReinstallablePkgs = [
        "rts"
        "ghc-heap"
        "ghc-prim"
        "integer-gmp"
        "integer-simple"
        "base"
        "deepseq"
        "array"
        "ghc-boot-th"
        "pretty"
        "template-haskell"
        "ghc-bignum"
        "exceptions"
        "stm"
        "ghc-boot"
        "ghc"
        "Cabal"
        "Win32"
        "array"
        "binary"
        "bytestring"
        "containers"
        "directory"
        "filepath"
        "ghc-boot"
        "ghc-compact"
        "ghc-prim"
        "hpc"
        "mtl"
        "parsec"
        "process"
        "text"
        "time"
        "transformers"
        "unix"
        "xhtml"
        "terminfo"
      ];

      ghcVersion = "ghc924";

      # We must keep the weeder version in sync with the version of
      # GHC we're using.
      weederVersion = "2.4.0";

      # Fourmolu updates often alter formatting arbitrarily, and we want to
      # have more control over this.
      fourmoluVersion = "0.7.0.1";

      forAllSupportedSystems = flake-utils.lib.eachSystem [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      forAllTestSystems = flake-utils.lib.eachSystem [
        "x86_64-linux"
        "aarch64-linux"
      ];

      overlays.primer = hacknix.lib.overlays.combine [
        haskell-nix.overlay
        (final: prev:
          let
            ghc8107Tools = final.haskell-nix.tools "ghc8107" {
              cabal-fmt = "latest";
              cabal-edit = "latest";
            };

            postgres-dev-password = "primer-dev";
            postgres-dev-base-url = "postgres://postgres:${postgres-dev-password}@localhost:5432";
            postgres-dev-primer-url = "${postgres-dev-base-url}/primer";

            sqitch = final.callPackage ./nix/pkgs/sqitch {
              postgresqlSupport = true;
            };

            pg_prove = final.perlPackages.TAPParserSourceHandlerpgTAP;

            scripts = final.lib.recurseIntoAttrs (final.callPackage ./nix/pkgs/scripts {
              sqitchDir = ./sqitch;
              inherit version;
            });

            # Temporary workaround for HLS issues until the next release.
            hls = (final.haskell-nix.cabalProject' {
              compiler-nix-name = ghcVersion;
              src = haskell-language-server;
              sha256map."https://github.com/pepeiborra/ekg-json"."7a0af7a8fd38045fd15fb13445bdcc7085325460" = "fVwKxGgM0S4Kv/4egVAAiAjV7QB5PBqMVMCfsv7otIQ=";

              modules = [
                {
                  inherit nonReinstallablePkgs;
                }
              ];
            }).hsPkgs.haskell-language-server.components.exes.haskell-language-server;

            primer = final.haskell-nix.cabalProject {
              compiler-nix-name = ghcVersion;
              src = ./.;
              modules = [
                {
                  inherit nonReinstallablePkgs;
                }
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
                  packages.primer.components.tests.primer-test.build-tools = [ final.haskell-nix.snapshots."lts-19.9".tasty-discover ];
                  packages.primer-rel8.components.tests.primer-rel8-test.build-tools = [
                    final.haskell-nix.snapshots."lts-19.9".tasty-discover
                    final.postgresql
                    final.primer-sqitch
                  ];
                  packages.primer-service.components.tests.service-test.build-tools = [
                    final.haskell-nix.snapshots."lts-19.9".tasty-discover
                    final.postgresql
                    final.primer-sqitch
                  ];
                }
                {
                  #TODO Haskell.nix would ideally pick this up from `cabal.project`.
                  # See: https://github.com/input-output-hk/haskell.nix/issues/1149#issuecomment-946664684
                  packages.primer.components.tests.primer-test.testFlags = [ "--size-cutoff=32768" ];
                  packages.primer-service.components.tests.service-test.testFlags = [ "--size-cutoff=32768" ];
                }
              ];

              shell = {
                exactDeps = true;
                withHoogle = true;

                tools = {
                  ghcid = "latest";

                  # Temporary workaround for HLS issues until the next release.
                  #haskell-language-server = "latest";

                  cabal = "latest";
                  hlint = exceptionsWorkaround "latest";
                  weeder = exceptionsWorkaround weederVersion;

                  fourmolu = fourmoluVersion;

                  # Not yet working with GHC 9.2.2.
                  #cabal-edit = "latest";
                  #cabal-fmt = "latest";

                  #TODO Explicitly requiring tasty-discover shouldn't be necessary - see the commented-out `build-tool-depends` in primer.cabal.
                  tasty-discover = "latest";
                };

                buildInputs = (with final; [
                  # Temporary workaround for HLS issues until the next release.
                  hls

                  nixpkgs-fmt
                  postgresql
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
                  pg_prove

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

                  ghc8107Tools.cabal-edit
                  ghc8107Tools.cabal-fmt
                ]);

                shellHook = ''
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

            run-primer = final.writeShellApplication {
              name = "run-primer";
              runtimeInputs = [
                final.primer-service
                final.primer-sqitch
              ];
              text = ''
                DATABASE_URL="${final.lib.primer.postgres-dev-primer-url}"
                export DATABASE_URL
                primer-sqitch deploy --verify db:$DATABASE_URL
                primer-service serve ${version} "$@"
              '';
            };

            primer-service-docker-image = final.dockerTools.buildLayeredImage {
              name = "primer-service";
              tag = version;
              contents = [
                scripts.primer-service-entrypoint
              ]
              ++ (with final; [
                # These are helpful for debugging broken images.
                bashInteractive
                coreutils
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
                    "org.opencontainers.image.revision" = self.rev or "dirty";
                  };

                  ExposedPorts = { "${toString port}/tcp" = { }; };

                  Env = [
                    # Needed for the `primer-service` banner.
                    "LANG=C.UTF-8"

                    # Sqitch will fail in a container if these are not
                    # set. Their specific values are not important.
                    "SQITCH_EMAIL=root@localhost"
                    "SQITCH_FULLNAME=Primer User"
                  ];
                };
            };
          in
          {
            lib = (prev.lib or { }) // {
              primer = (prev.lib.primer or { }) // {
                defaultServicePort = 8081;
                inherit postgres-dev-password;
                inherit postgres-dev-base-url;
                inherit postgres-dev-primer-url;
              };
            };

            inherit sqitch;
            inherit pg_prove;

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
              primer-pgtap-tests;

            inherit primer;

            primer-service = primerFlake.packages."primer-service:exe:primer-service";
            primer-openapi = primerFlake.packages."primer-service:exe:primer-openapi";

            inherit primer-openapi-spec;
            inherit run-primer;
            inherit primer-service-docker-image;

            inherit (ghc8107Tools) cabal-edit cabal-fmt;
            inherit hls;
          }
        )
      ];

      pkgsFor = system: import nixpkgs {
        inherit system;
        inherit (haskell-nix) config;
        overlays = [
          overlays.primer
        ];
      };
    in
    {
      inherit overlays;
    }

    // forAllSupportedSystems (system:
    let
      pkgs = pkgsFor system;

      # haskell.nix does a lot of heavy lifiting for us and gives us a
      # flake for our Cabal project with the following attributes:
      # `checks`, `apps`, and `packages`.
      primerFlake = pkgs.primer.flake { };

      weeder =
        let
          weederTool = pkgs.haskell-nix.tool ghcVersion "weeder" (exceptionsWorkaround weederVersion);
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

      pre-commit-hooks =
        let
          # Override the default nix-pre-commit-hooks tools with the version
          # we're using.
          haskellNixTools = pkgs.haskell-nix.tools ghcVersion {
            hlint = exceptionsWorkaround "latest";
            fourmolu = fourmoluVersion;

            # Not yet working with GHC 9.2.2.
            #cabal-fmt = "latest";
          };
        in
        pre-commit-hooks-nix.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            fourmolu.enable = true;
            cabal-fmt.enable = true;
            nixpkgs-fmt.enable = true;

            actionlint = {
              enable = true;
              name = "actionlint";
              entry = "${pkgs.actionlint}/bin/actionlint";
              language = "system";
              files = "^.github/workflows/";
            };
          };

          # Override the default nix-pre-commit-hooks tools with the version
          # we're using.
          tools = {
            inherit (pkgs) nixpkgs-fmt;
            inherit (pkgs) cabal-fmt;
          } // haskellNixTools;

          excludes = [
            "primer/test/outputs"
            "primer-service/test/outputs"
            ".buildkite/"
            "primer-service/primer-service.cabal"
          ];
        };

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
        cleanSourceWith
          {
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
      packages =
        {
          inherit (pkgs) primer-service primer-openapi-spec run-primer;
          inherit (pkgs)
            create-local-db
            deploy-local-db
            verify-local-db
            revert-local-db
            status-local-db
            log-local-db
            delete-local-db
            dump-local-db
            restore-local-db

            sqitch
            pg_prove
            primer-sqitch
            primer-pgtap-tests

            deploy-postgresql-container
            start-postgresql-container
            stop-postgresql-container;
        }
        // (pkgs.lib.optionalAttrs (system == "x86_64-linux" || system == "aarch64-linux") {
          inherit (pkgs) primer-service-docker-image;
        })
        // primerFlake.packages;

      checks =
        {
          source-code-checks = pre-commit-hooks;
          inherit weeder openapi-validate;

        }

        # Broken on NixOS. See:
        # https://github.com/hackworthltd/primer/issues/632
        // (pkgs.lib.optionalAttrs (system == "aarch64-darwin") {

          # Make sure HLS can typecheck our project.
          check-hls = pkgs.callPackage ./nix/pkgs/check-hls {
            src = onlyHaskellSrc;
            inherit version;

            # This is a bit of a hack, but we don't know a better way.
            inherit (primerFlake) devShell;
          };
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
          inherit (pkgs) run-primer primer-openapi-spec;

          inherit (pkgs)
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

            deploy-postgresql-container
            start-postgresql-container
            stop-postgresql-container;
        })
        // primerFlake.apps;

      defaultApp = self.apps.${system}.run-primer;

      inherit (primerFlake) devShell;
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
              packages.aarch64-linux
              packages.aarch64-darwin
              checks.x86_64-linux
              checks.aarch64-linux
              checks.aarch64-darwin
              tests.x86_64-linux
              tests.aarch64-linux
              devShell
            ]);
            meta.description = "Required CI builds";
          };
      }

      // forAllTestSystems (system: {
        tests =
          let
            pkgs = pkgsFor system;
          in
          (hacknix.lib.testing.nixos.importFromDirectory ./nixos-tests
            {
              inherit system pkgs;
            }
            {
              inherit (pkgs) primer-service-docker-image;
              inherit (pkgs) primer-sqitch pg_prove primer-pgtap-tests;
              inherit (pkgs.lib.primer) defaultServicePort;
              inherit version;
            });
      });

      ciJobs = hacknix.lib.flakes.recurseIntoHydraJobs self.hydraJobs;
    };
}
