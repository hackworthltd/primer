# NOTE:
#
# Most commands assume you're running this from the top-level `nix
# develop` shell.

build:
	cabal build

package-targets = test haddock clean

$(package-targets):
	cabal $@

update-tests:

build-wasm:
	wasm32-unknown-wasi-cabal build

test-wasm:
	wasm32-unknown-wasi-cabal test --test-wrapper=wasm32-test-runner

.PHONY: build $(package-targets) update-tests build-wasm test-wasm
