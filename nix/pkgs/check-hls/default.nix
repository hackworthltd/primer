{ stdenv
, lib
, src
, version
, devShell
}:

stdenv.mkDerivation {
  pname = "check-hls";
  inherit version;
  inherit src;

  # This ensures we have the proper version of GHC and all of
  # Primer's dependencies available in the derivation.
  inherit (devShell) buildInputs nativeBuildInputs;

  # Ensure that Cabal doesn't try to hit the network.
  CABAL_CONFIG = "${devShell.CABAL_CONFIG}";

  dontConfigure = true;
  dontBuild = true;

  doCheck = true;

  # haskell-language-server wants to create a ~/.cache directory,
  # but default $HOME is non-writable during a Nix build.
  preCheck = ''
    export HOME=$(mktemp -d)
  '';
  checkPhase = ''
    runHook preCheck
    haskell-language-server
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    touch $out
    runHook postInstall
  '';

  dontFixup = true;

  meta.platforms = lib.platforms.all;
}

