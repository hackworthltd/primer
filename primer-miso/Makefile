# NOTE:
#
# This Makefile assumes you're using the `nix develop` shell.

include ../makefiles/common.mk

# TODO The last line here shouldn't be necessary.
# For some reason, GHCID is failing to discover the right paths to watch.
develop-frontend:
	ghcid \
		-c "cabal repl --enable-multi-repl exe:primer-miso lib:primer-miso" \
		-W -T Main.main \
		--reload frontend/style.css \
		--reload exe --reload src --ignore-loaded \
