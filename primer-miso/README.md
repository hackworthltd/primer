# Primer Miso frontend

This directory contains the Wasm-based frontend for Primer. The commands below assume you are working in the repository root.

## Development environment

The frontend relies on the Wasm toolchain that is provided by the Nix "wasm" development shell. Enter it before installing dependencies or building assets:

```sh
nix develop .#wasm
```

Once inside the shell you can install the Node dependencies with either npm or pnpm:

```sh
npm install
# or
pnpm install
```

Running the install step inside the Nix shell ensures that the platform-specific Rspack bindings (e.g. `@rspack/binding-wasm32-wasi`) are downloaded correctly.

## Frontend build commands

The package.json scripts expose three Rspack entry points:

- `npm run build` – produces a development build (`rspack build --mode=development`).
- `npm run build:prod` – produces an optimized production build (`rspack build --mode=production`).
- `npm run watch` – starts an incremental build that watches the filesystem for changes (`rspack watch`).

These scripts only run Rspack. In normal workflows you should prefer the `build-frontend.sh` helper in this directory, which first produces the required WebAssembly artifacts and then calls Rspack. For example, the default `./build-frontend.sh` invocation is equivalent to running `npm run build` after the artifacts have been generated, while `./build-frontend.sh --mode production` aligns with `npm run build:prod`.

You can also forward custom Rspack commands through the script, e.g. `./build-frontend.sh --command watch` to drive the same watcher loop that `npm run watch` uses, but with the Wasm build steps included.

## Generated artifacts and troubleshooting

`build-frontend.sh` is responsible for creating the files that the frontend bundle expects under `frontend/generated/`:

- `bin.wasm` – built from the `primer-miso` executable via `wasm32-wasi-cabal list-bin`. In production mode the script additionally runs `wizer`, `wasm-opt`, and `wasm-tools strip`, and optionally compresses the result with `brotli`.
- `ghc_wasm_jsffi.js` – produced by running the GHC `post-link.mjs` script over the compiled Wasm binary.

If you run `npm run build`/`watch` directly without `build-frontend.sh`, Rspack will fail when these files are missing. To fix that:

1. Re-enter the Nix Wasm shell (`nix develop .#wasm`).
2. Run `./build-frontend.sh --command build` (or `--command watch`) once to regenerate the artifacts.
3. Re-run your desired Rspack command.

If `ghc_wasm_jsffi.js` is not generated, double-check that you are inside the Wasm shell—the script exits early with a helpful error if `post-link.mjs` cannot be found. You can also remove the `frontend/generated/` directory and rerun `build-frontend.sh` to force a clean rebuild.
