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

.PHONY: build $(project-targets) $(package-targets) clean

# Disabled until Weeder is fixed with haskell.nix

# weeder: build
# 	weeder
# 	@echo "No issues found."
