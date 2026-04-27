{ nix
, wrangler
, writeShellApplication
,
}:

writeShellApplication {
  name = "deploy-to-cloudflare";

  runtimeInputs = [
    nix
    wrangler
  ];

  text = ''
    if [ "$#" -ne 1 ]; then
      echo "Usage: deploy-to-cloudflare <wrangler-config>" >&2
      exit 2
    fi

    wrangler_config="$1"

    if [ ! -f "$wrangler_config" ]; then
      echo "Wrangler config not found: $wrangler_config" >&2
      exit 1
    fi

    dist_path="$(nix build --print-build-logs --no-update-lock-file --no-link --print-out-paths .#primer-miso-dist)"
    wrangler deploy --config "$wrangler_config" --assets "$dist_path" --keep-vars
  '';
}
