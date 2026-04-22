import * as browser_wasi_shim from "./browser_wasi_shim/index.js";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const wasi = new browser_wasi_shim.WASI(
  [],
  ["GHCRTS=-H64m"],
  [
    new browser_wasi_shim.OpenFile(new browser_wasi_shim.File([])),
    browser_wasi_shim.ConsoleStdout.lineBuffered(console.log),
    browser_wasi_shim.ConsoleStdout.lineBuffered(console.warn),
  ],
  { debug: false },
);

const exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("app.wasm"), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(exports),
});
Object.assign(exports, instance.exports);

wasi.initialize(instance);
await instance.exports.hs_start();
