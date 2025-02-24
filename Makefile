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
	$(MAKE) -C primer-selda $@
	$(MAKE) -C primer-service $@
	$(MAKE) -C primer-benchmark $@

openapi.json: build
	cabal run -v0 primer-service:exe:primer-openapi > $@
	openapi-generator-cli validate --recommend -i $@

develop-frontend:
	$(MAKE) -C primer-miso develop-frontend

clean:
	cabal clean
	rm -f openapi.json

.PHONY: build $(project-targets) $(package-targets) openapi.json clean

# Disabled until Weeder is fixed with haskell.nix

# weeder: build
# 	weeder
# 	@echo "No issues found."
