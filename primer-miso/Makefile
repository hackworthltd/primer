# NOTE:
#
# This Makefile assumes you're using the `nix develop` shell.

include ../makefiles/common.mk

frontend: build-wasm
	nix run ..#primer-miso-build-frontend -- --frontend-dir frontend --out-dir dist

frontend-prod: build-wasm
	nix run ..#primer-miso-build-frontend -- --prod --frontend-dir frontend --out-dir dist

serve: frontend
	simple-http-server -i -p 8000 -- dist
