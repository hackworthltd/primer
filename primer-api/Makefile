# NOTE:
#
# Most commands assume you're running this from the top-level `nix
# develop` shell.

build:
	cabal build

wasm32-build:
	wasm32-wasi-cabal build

configure:
	cabal configure

wasm32-configure:
	wasm32-wasi-cabal configure

check:	test

wasm32-check:	wasm32-test

wasm32-test:
	wasm32-wasi-cabal test

docs:
	cabal haddock

clean:
	cabal clean

wasm32-clean:
	wasm32-wasi-cabal clean

bench:

realclean:

deps:

.PHONY: build bench configure test docs clean realclean deps
