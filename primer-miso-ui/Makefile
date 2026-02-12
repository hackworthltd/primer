# NOTE:
#
# This Makefile assumes you're using the `nix develop` shell.

include ../makefiles/common.mk

# TODO The last line here shouldn't be necessary.
# For some reason, GHCID is failing to discover the right paths to watch.
develop-ui-demo:
	ghcid \
		-c "cabal repl --enable-multi-repl exe:primer-miso-ui-demo lib:primer-miso-ui" \
		-W -T Main.main \
		--reload exe --reload src --ignore-loaded
