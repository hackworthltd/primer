{ lib
, pkgsBuildBuild
, writeShellScriptBin
}:

writeShellScriptBin "wasm32-test-runner" ''
  exec ${lib.getExe pkgsBuildBuild.wasmtime} --dir test::test "$@"
''
