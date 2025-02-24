# NOTE:
#
# This Makefile assumes you're using the `nix develop` shell.

include ../makefiles/common.mk

develop-frontend:
	ghcid -c "cabal repl exe:primer-miso" -W -T ':main'
