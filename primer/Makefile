# NOTE:
#
# This Makefile assumes you're using the `nix develop` shell.

include ../makefiles/common.mk

# Update any test files which differ from the expected result.
serialization-outputs:
	cabal run primer-test -- -p Serialization.encode --accept
available-action-outputs:
	cabal run primer-test -- -p Action.Available --accept

update-tests:	serialization-outputs available-action-outputs

.PHONY: update-tests serialization-outputs available-action-outputs
