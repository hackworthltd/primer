{ coreutils
, gnugrep
, pkgsCross
, runCommand
, writeShellScriptBin
, cabalTool
}:
let
  wasm-dummy-liblibdl = runCommand "liblibdl"
    {
      nativeBuildInputs = [ pkgsCross.wasi32.buildPackages.llvmPackages.clang ];
    }
    ''
      mkdir -p $out/lib
      echo 'void __liblibdl_stub(void) {}' | wasm32-unknown-wasi-cc -shared -x c - -o $out/lib/liblibdl.so 2>/dev/null
    '';

  forced-wasm-ghc-pkg = writeShellScriptBin "ghc-pkg" ''
    exec wasm32-unknown-wasi-ghc-pkg "$@"
  '';
in
writeShellScriptBin "wasm32-unknown-wasi-cabal" ''
  PATH="${forced-wasm-ghc-pkg}/bin:$PATH" \
  LD_LIBRARY_PATH="${wasm-dummy-liblibdl}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
  NIX_LDFLAGS=$(echo "''${NIX_LDFLAGS:-}" | ${coreutils}/bin/tr ' ' '\n' | ${gnugrep}/bin/grep -v 'libffi-[0-9]' | ${coreutils}/bin/tr '\n' ' ') \
  NIX_LDFLAGS_FOR_TARGET=$(echo "''${NIX_LDFLAGS_FOR_TARGET:-}" | ${coreutils}/bin/tr ' ' '\n' | ${gnugrep}/bin/grep -v 'libffi-[0-9]' | ${coreutils}/bin/tr '\n' ' ') \
  exec ${cabalTool}/bin/cabal \
    --with-ghc=wasm32-unknown-wasi-ghc \
    --with-compiler=wasm32-unknown-wasi-ghc \
    --with-ghc-pkg=wasm32-unknown-wasi-ghc-pkg \
    --with-hsc2hs=wasm32-unknown-wasi-hsc2hs \
    $(builtin type -P "wasm32-unknown-wasi-pkg-config" &> /dev/null && echo "--with-pkg-config=wasm32-unknown-wasi-pkg-config") \
    "$@"
''
