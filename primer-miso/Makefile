# NOTE:
#
# This Makefile assumes you're using the `nix develop` shell.

include ../makefiles/common.mk

frontend: build-wasm
	nix run ..#primer-miso-build-frontend -- --frontend-dir frontend --out-dir dist

frontend-prod: build-wasm
	nix run ..#primer-miso-build-frontend -- --prod --frontend-dir frontend --out-dir dist

# Note the `--nocache`: without it, `simple-http-server` sets
# `Cache-Control: public, max-age=604800` on everything it serves, so browsers
# will happily serve a week-old `app.wasm` without ever revalidating, and you
# have to clear the cache by hand after every rebuild.
serve: frontend
	simple-http-server -i --nocache -p 8000 -- dist
