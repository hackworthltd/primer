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
	cabal test --test-options=--size-cutoff=10000000000

# Run the encoding tests, and update any files which differ from the expected result.
generate-fixtures:
	cabal run primer-test -- -p Serialisation.encode --accept

docs:
	cabal haddock

clean:
	cabal clean

realclean:

deps:

.PHONY: build configure test docs clean realclean deps
