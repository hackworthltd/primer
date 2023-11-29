# WebAssembly (Wasm) support

**Note**: WebAssembly support is currently very preliminary.

For horizontal scalability reasons, we would like to run the `primer`
and `primer-api` packages in the student's browser, rather than on a
backend server. Therefore, we'd like to compile these packages to a
`wasm32-wasi` target and call the (native) Primer API from TypeScript.

Currently, we can compile these two packages to `wasm32-wasi`, but
with the following caveats:

1. Neither `haskell.nix` nor `nixpkgs.haskellPackages` support the
   `wasm32-wasi` cross-target at the moment, so we can only build Wasm
   targets directly via `wasm32-wasi-cabal` and `wasm32-wasi-ghc`. For
   interactive development, we provide a special `nix develop` shell
   which provides the necessary tools:

   ```sh
   nix develop .#wasm
   ```

   Note that the required tools are currently only available for
   `x86_64-linux` Nix systems, so the special `wasm` Nix shell only
   exists for that platform.

2. Once you're in the special Wasm shell, it's advisable to use the
   special `wasm32` `Makefile` targets. To build the libraries, run:

   ```sh
   make wasm32-configure
   make wasm32-build
   ```

   To build the tests and run them using the `wasmtime` runtime, run:

   ```sh
   make wasm32-test
   ```

   The `wasm32-test`, in particular, needs to run several steps that
   you'd otherwise need to run by hand in order to work around
   `wasmtime` issues.
