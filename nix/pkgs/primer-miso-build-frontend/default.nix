{ bash
, browser-wasi-shim
, lib
, makeWrapper
, primer-miso-frontend-tools
, primer-miso-node-modules
, stdenvNoCC
, wasmGhc
, buildFrontendScript ? ../../primer-miso-build-frontend
}:

stdenvNoCC.mkDerivation {
  pname = "primer-miso-build-frontend";
  version = "1.0";

  src = buildFrontendScript;

  nativeBuildInputs = [
    makeWrapper
  ];

  dontUnpack = true;

  installPhase = ''
    runHook preInstall

    install -Dm755 $src $out/bin/primer-miso-build-frontend
    wrapProgram $out/bin/primer-miso-build-frontend \
      --prefix PATH : ${lib.makeBinPath [
        bash
        primer-miso-frontend-tools
        wasmGhc
      ]} \
      --set BROWSER_WASI_SHIM ${browser-wasi-shim} \
      --set BUILD_FRONTEND_NAME primer-miso-build-frontend \
      --prefix NODE_PATH : ${primer-miso-node-modules}

    runHook postInstall
  '';
}
