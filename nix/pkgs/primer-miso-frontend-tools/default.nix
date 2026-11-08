{ binaryen
, brotli
, buildEnv
, coreutils
, gnugrep
, nodejs_22
, wasm-tools
, wasm32-unknown-wasip1-cabal
, wizer
}:

buildEnv {
  name = "primer-miso-frontend-tools";
  paths = [
    binaryen
    brotli
    coreutils
    gnugrep
    nodejs_22
    wasm-tools
    wasm32-unknown-wasip1-cabal
    wizer
  ];
}
