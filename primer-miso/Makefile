# NOTE:
#
# This Makefile assumes you're using the `nix develop` shell.

include ../makefiles/common.mk

frontend: build-wasm
	./build-frontend.sh

frontend-prod: build-wasm
	./build-frontend.sh -Oz

serve: frontend
	simple-http-server -i -p 8000 -- dist
