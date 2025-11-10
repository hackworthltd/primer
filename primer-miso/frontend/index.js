import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";
import ghc_wasm_jsffiUrl from "./generated/ghc_wasm_jsffi.js?asset";
import "./style.css";

const args = [];
const env = [];
const fds = [
  new OpenFile(new File([])), // stdin
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);

const wasmUrl = new URL("./generated/bin.wasm", import.meta.url);
const ghcModule = await import(/* webpackIgnore: true */ ghc_wasm_jsffiUrl);
const ghc_wasm_jsffi =
  ghcModule && typeof ghcModule === "object" && "default" in ghcModule ? ghcModule.default : ghcModule;
if (typeof ghc_wasm_jsffi !== "function") {
  throw new Error("Failed to load ghc_wasm_jsffi module");
}

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch(wasmUrl), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
await instance.exports.hs_start();
