#!/usr/bin/env bash

set -e

if [ $# -eq 0 ]; then
    echo "Building for dev"
    dev_mode=true
else
    echo "Building for prod"
    dev_mode=false
fi

rm -rf dist
mkdir dist

hs_wasm_path=$(wasm32-wasi-cabal list-bin -v0 exe:primer-miso-ui-demo)

ghc_wasm_jsffi="dist/ghc_wasm_jsffi.js"

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
                                   --input "$hs_wasm_path" --output "$ghc_wasm_jsffi"

if ! [ -f "$ghc_wasm_jsffi" ] ; then
    echo "post-link.mjs didn't produce a $ghc_wasm_jsffi file. Make sure you're in the Nix Wasm shell."
    exit 1
fi

if $dev_mode; then
    cp "$hs_wasm_path" dist/bin.wasm
else
    env -i GHCRTS=-H64m "$(type -P wizer)" --allow-wasi --wasm-bulk-memory true --inherit-env true --init-func _initialize -o dist/bin.wasm "$hs_wasm_path"
    wasm-opt ${1+"$@"} dist/bin.wasm -o dist/bin.wasm
    wasm-tools strip -o dist/bin.wasm dist/bin.wasm
    brotli --rm --best dist/bin.wasm -o dist/bin.wasm.br
    mv dist/bin.wasm.br dist/bin.wasm
fi

cp static/*.html dist/
cp static/*.js dist
cp -r static/deps dist/
