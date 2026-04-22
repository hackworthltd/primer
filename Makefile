# NOTE:
#
# This Makefile assumes you're using the `nix develop` shell.

build:
	cabal build all

project-targets = test bench haddock

$(project-targets):
	cabal $@ all

package-targets = update-tests

$(package-targets):
	$(MAKE) -C primer $@
	$(MAKE) -C primer-api $@
	$(MAKE) -C primer-miso $@
	$(MAKE) -C primer-benchmark $@

clean:
	cabal clean

# Wasm

build-wasm:
	wasm32-unknown-wasi-cabal build all

test-wasm:
	wasm32-unknown-wasi-cabal test all --test-wrapper=wasm32-test-runner

frontend-targets = frontend frontend-prod serve

$(frontend-targets):
	$(MAKE) -C primer-miso $@

.PHONY: build $(project-targets) $(package-targets) clean build-wasm test-wasm $(frontend-targets)

# Disabled until Weeder is fixed with haskell.nix

# weeder: build
# 	weeder
# 	@echo "No issues found."
