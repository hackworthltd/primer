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
	$(MAKE) -C primer-miso-ui $@
	$(MAKE) -C primer-benchmark $@

develop-frontend:
	$(MAKE) -C primer-miso develop-frontend

develop-ui-demo:
	$(MAKE) -C primer-miso-ui develop-ui-demo

format:
	nix develop .#treefmt -c treefmt

clean:
	cabal clean

.PHONY: build $(project-targets) $(package-targets) format clean

# Disabled until Weeder is fixed with haskell.nix

# weeder: build
# 	weeder
# 	@echo "No issues found."
