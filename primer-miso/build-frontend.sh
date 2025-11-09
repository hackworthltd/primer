#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

if [ $# -eq 0 ]; then
    echo "Building for dev"
    dev_mode=true
    rspack_mode=development
else
    echo "Building for prod"
    dev_mode=false
    rspack_mode=production
fi

rm -rf dist

generated_dir="frontend/generated"
mkdir -p "$generated_dir"

hs_wasm_path=$(wasm32-wasi-cabal list-bin -v0 exe:primer-miso)

ghc_wasm_jsffi="$generated_dir/ghc_wasm_jsffi.js"

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
    --input "$hs_wasm_path" --output "$ghc_wasm_jsffi"

if ! [ -f "$ghc_wasm_jsffi" ] ; then
    echo "post-link.mjs didn't produce a $ghc_wasm_jsffi file. Make sure you're in the Nix Wasm shell."
    exit 1
fi

bin_wasm="$generated_dir/bin.wasm"

if $dev_mode; then
    cp "$hs_wasm_path" "$bin_wasm"
else
    wizer --allow-wasi --wasm-bulk-memory true --init-func _initialize -o "$bin_wasm" "$hs_wasm_path"
    wasm-opt ${1+"$@"} "$bin_wasm" -o "$bin_wasm"
    wasm-tools strip -o "$bin_wasm" "$bin_wasm"
    if command -v brotli >/dev/null 2>&1; then
        brotli --keep --best "$bin_wasm" -o "$generated_dir/bin.wasm.br"
    fi
fi

npm exec -- rspack build --mode "$rspack_mode"
