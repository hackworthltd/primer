# Adapted from
# https://github.com/hercules-ci/flake-parts/blob/7c7a8bce3dffe71203dcd4276504d1cb49dfe05f/modules/packages.nix

{ config, lib, flake-parts-lib, ... }:
let
  inherit (lib)
    mkOption
    types
    ;
  inherit (flake-parts-lib)
    mkTransposedPerSystemModule
    ;
in
mkTransposedPerSystemModule {
  name = "benchmarks";
  option = mkOption {
    type = types.lazyAttrsOf types.package;
    default = { };
    description = ''
      An attribute set of benchmarks to be built by [`nix build`](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-build.html).
      `nix build .#benchmarks.<name>` will build `benchmarks.<name>`.
    '';
  };
  file = ./benchmarks.nix;
}
