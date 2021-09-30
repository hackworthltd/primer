# NOTE:
#
# Most commands assume you're running this from the top-level `nix
# develop` shell.

targets = build configure check test generate-fixtures docs clean realclean deps

$(targets):
	$(MAKE) -C primer $@
	$(MAKE) -C primer-selda $@
	$(MAKE) -C primer-service $@

weeder:
	cabal build all --enable-benchmarks --enable-tests
	weeder
	@echo "No issues found."

openapi.json: build
	cabal run -v0 primer-service:exe:primer-openapi > $@

.PHONY: $(targets) weeder
