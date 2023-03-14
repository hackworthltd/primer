{
  description = "trying out ghc+wasm";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.ghc-wasm.url = "git+https://gitlab.haskell.org/ghc/ghc-wasm-meta";

  outputs = { self, nixpkgs, ghc-wasm }: let
   system = "x86_64-linux";
   pkgs = import nixpkgs { inherit system; };
  in {
    devShell.x86_64-linux = pkgs.mkShell {
      packages = [
        ghc-wasm.packages.${system}.wasm32-wasi-cabal
        ghc-wasm.packages.${system}.wasm32-wasi-ghc-gmp
        ghc-wasm.packages.${system}.wasmtime
      ];
    };
  };
}
