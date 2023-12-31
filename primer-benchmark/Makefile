# NOTE:
#
# Most commands assume you're running this from the top-level `nix
# develop` shell.

wasm32-primer-benchmark-test	= $(shell wasm32-wasi-cabal list-bin -v0 test:primer-benchmark-test)
wasm32-primer-benchmark-test-opt	= $(shell wasm32-wasi-cabal list-bin -O2 -v0 test:primer-benchmark-test)

build:
	cabal build

wasm32-build:
	wasm32-wasi-cabal build

wasm32-build-opt:
	wasm32-wasi-cabal build -O2

configure:
	cabal configure

wasm32-configure:
	wasm32-wasi-cabal configure

check:	test

wasm32-check:	wasm32-test

test:
	cabal test

wasm32-test:
	wasm32-wasi-cabal build test:primer-benchmark-test
	wasmtime --dir test::test "$(wasm32-primer-benchmark-test)"

wasm32-test-opt:
	wasm32-wasi-cabal build -O2 test:primer-benchmark-test
	wasmtime --dir test::test "$(wasm32-primer-benchmark-test-opt)"

bench:
	cabal bench

wasm32-bench: wasm32-build-opt
	wasmtime "$(wasm32-primer-benchmark-test-opt)"

docs:
	cabal haddock

clean:
	cabal clean

realclean:

deps:

.PHONY: build bench configure test docs clean realclean deps
