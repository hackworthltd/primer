{ browser-wasi-shim
, primer-miso-build-frontend
, primer-miso-wasm
, stdenvNoCC
, version
, frontendSrc ? ../../../primer-miso/frontend
}:

stdenvNoCC.mkDerivation {
  pname = "primer-miso-dist";
  inherit version;

  nativeBuildInputs = [
    primer-miso-build-frontend
  ];

  dontUnpack = true;

  buildPhase = ''
    runHook preBuild

    export HOME=$(mktemp -d)
    export XDG_CACHE_HOME="$HOME/.cache"

    primer-miso-build-frontend \
      --prod \
      --out-dir "$out" \
      --frontend-dir ${frontendSrc} \
      --browser-wasi-shim ${browser-wasi-shim} \
      --wasm ${primer-miso-wasm}/bin/primer-miso.wasm

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    runHook postInstall
  '';
}
