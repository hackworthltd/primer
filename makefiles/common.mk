# NOTE:
#
# Most commands assume you're running this from the top-level `nix
# develop` shell.

build:	configure
	cabal build

package-targets = configure test haddock clean

$(package-targets):
	cabal $@

update-tests:

.PHONY: build $(package-targets) update-tests
