# NOTE:
#
# Most commands assume you're running this from the top-level `nix
# develop` shell.

targets = build configure check test bench generate-fixtures docs clean realclean deps

$(targets):
	$(MAKE) -C primer $@
	$(MAKE) -C primer-api $@
	$(MAKE) -C primer-selda $@
	$(MAKE) -C primer-service $@
	$(MAKE) -C primer-benchmark $@

wasm32-update:
	wasm32-wasi-cabal update

wasm32 = wasm32-build wasm32-build-opt wasm32-configure wasm32-check wasm32-test wasm32-test-opt wasm32-clean

$(wasm32):
	$(MAKE) -C primer $@
	$(MAKE) -C primer-api $@
	$(MAKE) -C primer-benchmark $@

weeder:
	cabal build all --enable-benchmarks --enable-tests
	weeder
	@echo "No issues found."

openapi.json: build
	cabal run -v0 primer-service:exe:primer-openapi > $@
	openapi-generator-cli validate --recommend -i $@

.PHONY: $(targets) $(wasm32-targets) weeder
