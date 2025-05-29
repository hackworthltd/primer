// curl https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/debug.js -o primer-miso/frontend/debug.js
// curl https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/fd.js -o primer-miso/frontend/fd.js
// curl https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/fs_mem.js -o primer-miso/frontend/fs_mem.js
// curl https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/fs_opfs.js -o primer-miso/frontend/fs_opfs.js
// curl https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js -o primer-miso/frontend/browser_wasi_shim.js
// curl https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/strace.js -o primer-miso/frontend/strace.js
// curl https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/wasi_defs.js -o primer-miso/frontend/wasi_defs.js
// curl https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/wasi.js -o primer-miso/frontend/wasi.js
// curl https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/wasi.js -o primer-miso/frontend/wasi.js
import { WASI, OpenFile, File, ConsoleStdout } from "./browser_wasi_shim.js";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const args = [];
const env = [];
const fds = [
  new OpenFile(new File([])), // stdin
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("bin.wasm"), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
await instance.exports.hs_start();
