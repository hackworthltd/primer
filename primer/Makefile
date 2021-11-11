# NOTE:
#
# Most commands assume you're running this from the top-level `nix
# develop` shell.

build:
	cabal build

configure:
	cabal configure

check:	test

test:
	cabal test

# Update any test files which differ from the expected result.
serialization-outputs:
	cabal run primer-test -- -p Serialization.encode --accept
available-action-outputs:
	cabal run primer-test -- -p Action.Available --accept

docs:
	cabal haddock

clean:
	cabal clean

realclean:

deps:

.PHONY: build configure test docs clean realclean deps
