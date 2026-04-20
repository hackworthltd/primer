Future WebAssembly backend features not yet available in a released GHC. These have been manually backported for Stable Haskell GHC 9.14.1, with tests removed in order to minimise conflicts.

| Description                             | File                                                       | PR                                                                |
| --------------------------------------- | ---------------------------------------------------------- | ----------------------------------------------------------------- |
| Static file server in GHCI browser mode | [static-assets-server.patch](./static-assets-server.patch) | [link](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/15574) |
| Open browser in GHCI browser mode       | [open-browser-command.patch](./open-browser-command.patch) | none yet                                                          |
| Allow FFI exports in GHCI               | [ghci-ffi-export.patch](./ghci-ffi-export.patch)           | [link](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/15648) |
