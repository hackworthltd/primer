This is a WIP branch exploring building primer with ghc's wasm backend.
This uses the https://gitlab.haskell.org/ghc/ghc-wasm-meta flake to provide binaries.

## Contrasting with the 'main' branch:
We do not use haskell.nix, and only support `x86_64-linux`.
The flake only provides cabal, ghc and wasmtime, not any libraries.
The flake also only provides a shell, not any packages -- you are expected to use cabal in a devshell to build (this will imperatively manage haskell dependencies).

## Supported packages
We currently only support building the core primer library `lib:primer` and nothing else.
This is because of failures in building some dependencies

## Unbuildable dependencies for `lib:primer-testlib`
- happy
- terminal-size

## Modified dependencies
### for `lib:primer`
#### extra
We remove its 'clock' dependency and all related functonality (which we do not use).
This is due to 'clock' not building with ghc's wasm backend.
#### logging-effect
We remove some instances for deprecated types which have been removed in transformers-0.7
#### uuid
We remove its 'network-info' and all related functionality (which we do not use).
We remove its 'entropy' dependency, and replace it with 'random' -- this may be less secure but we don't care (we do not rely on its security guarentees).

## Running
Enter a nix devshell: `nix develop`.
You can use cabal as normal (more or less -- some subcommands are not supported), but it has a prefixed name:
`wasm32-wasi-cabal update`
`wasm32-wasi-cabal build lib:primer`
If you had built an executable, you could run via `wasmtime`, if you give it a path to the binary (NB: running with cabal run does not work):
`wasmtime $(wasm32-wasi-cabal list-bin test:primer-test)`
