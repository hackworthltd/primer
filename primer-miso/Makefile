# NOTE:
#
# This Makefile assumes you're using the `nix develop` shell.

include ../makefiles/common.mk

frontend: build-wasm
	./build-frontend.sh --frontend-dir frontend --out-dir dist

frontend-prod: build-wasm
	./build-frontend.sh --prod --frontend-dir frontend --out-dir dist

serve: frontend
	http-server dist -c-1 -o $(if $(PORT),-p $(PORT))

# Fast reloading via Wasm GHCI "browser mode".
# When we're on GHC 9.16, we'll be able to remove the separate asset server in favour of `-fghci-browser-assets-dir`.
dev:
	http-server frontend --cors -c-1 --silent -p 8002 & trap 'kill $$!' EXIT; \
	ghcid -r --command \
		"wasm32-unknown-wasip1-cabal repl exe:primer-miso -finteractive --repl-options='-fghci-browser -fghci-browser-port=8001 -fghci-browser-redirect-wasi-console'"
# Multi-component variant, i.e. loads all components in to GHCI so that we can work on the core simultaneously.
# Some day this should be the default, but it's just a preview for now, until various upstream improvements land.
# The main thing is that reloads are significantly slower.
# It's annoying that we have to list `primer-miso`'s component dependencies and their source directories manually.
# Also, `--repl-options` doesn't work properly with multiple components, so we use environment variables instead.
dev-multi:
	http-server frontend --cors -c-1 --silent -p 8002 & trap 'kill $$!' EXIT; \
	GHCI_BROWSER=1 GHCI_BROWSER_PORT=8001 GHCI_BROWSER_REDIRECT_WASI_CONSOLE=1 \
	ghcid -Wr --setup ':module + *MyMain' --reload ../primer/src --reload ../primer-api/src --command \
		"wasm32-unknown-wasip1-cabal repl --enable-multi-repl exe:primer-miso lib:primer lib:primer-api -finteractive"
