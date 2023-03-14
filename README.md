This is a WIP branch exploring building primer with ghc's wasm backend.
This uses the https://gitlab.haskell.org/ghc/ghc-wasm-meta flake to provide binaries.

## Contrasting with the 'main' branch:
We do not use haskell.nix, and only support `x86_64-linux`.
The flake only provides cabal, ghc and wasmtime, not any libraries.
The flake also only provides a shell, not any packages -- you are expected to use cabal in a devshell to build (this will imperatively manage haskell dependencies).

## Supported packages
We currently do not support building any primer packages.

## Running
Enter a nix devshell: `nix develop`.
You can use cabal as normal (more or less -- some subcommands are not supported), but it has a prefixed name:
`wasm32-wasi-cabal update`
`wasm32-wasi-cabal build lib:primer`
If you had built an executable, you could run via `wasmtime`, if you give it a path to the binary (NB: running with cabal run does not work):
`wasmtime $(wasm32-wasi-cabal list-bin test:primer-test)`
