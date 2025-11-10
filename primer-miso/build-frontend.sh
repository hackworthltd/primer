#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

rspack_mode=${RSPACK_MODE:-${NODE_ENV:-development}}
bundler_command=${RSPACK_COMMAND:-build}
wasm_opt_args=()

while [ $# -gt 0 ]; do
    case "$1" in
        --mode)
            if [ $# -lt 2 ]; then
                echo "--mode requires an argument" >&2
                exit 1
            fi
            rspack_mode="$2"
            shift 2
            ;;
        --mode=*)
            rspack_mode="${1#*=}"
            shift
            ;;
        --command)
            if [ $# -lt 2 ]; then
                echo "--command requires an argument" >&2
                exit 1
            fi
            bundler_command="$2"
            shift 2
            ;;
        --command=*)
            bundler_command="${1#*=}"
            shift
            ;;
        --)
            shift
            wasm_opt_args+=("$@")
            break
            ;;
        *)
            wasm_opt_args+=("$1")
            shift
            ;;
    esac
done

case "$rspack_mode" in
    development|dev)
        dev_mode=true
        rspack_mode=development
        ;;
    production|prod)
        dev_mode=false
        rspack_mode=production
        ;;
    *)
        echo "Unknown Rspack mode: $rspack_mode" >&2
        exit 1
        ;;
esac

case "$bundler_command" in
    build|watch|serve)
        ;;
    *)
        echo "Unsupported bundler command: $bundler_command" >&2
        exit 1
        ;;
esac

echo "Running frontend build pipeline: mode=$rspack_mode command=$bundler_command"

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
    if [ ${#wasm_opt_args[@]} -gt 0 ]; then
        wasm-opt "${wasm_opt_args[@]}" "$bin_wasm" -o "$bin_wasm"
    fi
    wasm-tools strip -o "$bin_wasm" "$bin_wasm"
    if command -v brotli >/dev/null 2>&1; then
        brotli --keep --best "$bin_wasm" -o "$generated_dir/bin.wasm.br"
    fi
fi

env NODE_ENV="$rspack_mode" npx rspack "$bundler_command" --mode "$rspack_mode"
