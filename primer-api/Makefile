# NOTE:
#
# Most commands assume you're running this from the top-level `nix
# develop` shell.

wasm32-primer-api-test	= $(shell wasm32-wasi-cabal list-bin -v0 test:primer-api-test)
wasm32-primer-api-test-opt	= $(shell wasm32-wasi-cabal list-bin -O2 -v0 test:primer-api-test)

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

discover-wasm32-tests:
	tasty-discover test/Test.hs _ test/TestsWasm32.hs --tree-display

wasm32-test: wasm32-build discover-wasm32-tests
	wasm32-wasi-cabal build test:primer-api-test
	wasmtime --dir test::test "$(wasm32-primer-api-test)"

wasm32-test-opt: wasm32-build-opt discover-wasm32-tests
	wasm32-wasi-cabal build -O2 test:primer-api-test
	wasmtime --dir test::test "$(wasm32-primer-api-test-opt)"

docs:
	cabal haddock

clean:
	cabal clean

wasm32-clean:
	wasm32-wasi-cabal clean

bench:

realclean:

deps:

.PHONY: build bench configure test docs clean realclean deps discover-wasm32-tests
