{ hostPkgs
, ...
}:
let
  dbPassword = "foopass";
  dbUser = "primer";
  dbName = "primer";
  dbUrl = "postgres://${dbUser}:${dbPassword}@postgres:5432/${dbName}";

  port = toString hostPkgs.lib.primer.defaultServicePort;
  version = hostPkgs.lib.primer.version;
in
{
  nodes = {
    postgres = { pkgs, config, ... }: {
      services.postgresql = {
        enable = true;
        package = pkgs.postgresql;
        enableTCPIP = true;
        authentication = ''
          hostnossl ${dbName} ${dbUser} 192.168.0.0/16 md5
        '';
        initialScript = pkgs.writeText "postgresql-init.sql" ''
          CREATE DATABASE ${dbName} TEMPLATE template0 ENCODING UTF8;
          CREATE USER ${dbUser} WITH PASSWORD '${dbPassword}';
          GRANT ALL PRIVILEGES ON DATABASE ${dbName} TO ${dbUser};
        '';
      };

      # This is essential, or else Sqitch will fail.
      time.timeZone = "UTC";

      networking.firewall.allowedTCPPorts = [ 5432 ];

      environment.systemPackages = with pkgs; [
        primer-sqitch
      ];
    };

    primer = { pkgs, config, ... }:
      let
        versionCheck = pkgs.writeShellApplication {
          name = "primer-version-check";
          text = ''
            RESULT=$(curl http://localhost:${port}/api/version | jq -r)
            if [ "$RESULT" != "${version}" ]; then
              echo "Expected primer-service version ${version}, but got $RESULT" >& 2
              exit 1
            fi
          '';
        };
      in
      {
        # This is essential, or else Sqitch will fail.
        time.timeZone = "UTC";

        # Default VM size is too small for our container.
        virtualisation = {
          diskSize = 2048;
          memorySize = 1024;
        };

        virtualisation.oci-containers = {
          containers.primer-service = {

            # Note: we need to use `hostPkgs` here rather than `pkgs`,
            # for some reason I don't understand. It seems to have
            # something to do with the haskell.nix overlay, because
            # other packages that are in our overlay, but don't rely
            # on haskell.nix, work fine from `pkgs`.
            image = "primer-service:${hostPkgs.primer-service-docker-image.imageTag}";
            imageFile = hostPkgs.primer-service-docker-image;

            ports = [ "${port}:${port}" ];
            extraOptions = [ "--network=host" ];
            environment = {
              DATABASE_URL = dbUrl;
            };
          };
        };

        # If the container doesn't start cleanly, the test has failed.
        systemd.services.podman-primer-service.serviceConfig.Restart = pkgs.lib.mkForce "no";

        # Make sure we can see container failures.
        systemd.services.podman-primer-service.serviceConfig.StandardOutput = pkgs.lib.mkForce "journal";
        systemd.services.podman-primer-service.serviceConfig.StandardError = pkgs.lib.mkForce "journal";

        # We want to manually start and stop the container.
        systemd.services.podman-primer-service.wantedBy = pkgs.lib.mkForce [ ];

        environment.systemPackages = with pkgs; [
          curl
          jq
          podman
          versionCheck
        ];
      };
  };

  testScript = { nodes, ... }:
    ''
      postgres.start();
      postgres.wait_for_unit("postgresql.service")

      primer.start();
      primer.systemctl("start podman-primer-service.service")
      primer.wait_for_unit("podman-primer-service.service")

      with subtest("fails if the database hasn't been deployed"):
          primer.sleep(5)
          primer.fail("podman healthcheck run primer-service")
          primer.systemctl("stop podman-primer-service.service")

      postgres.succeed(
          "primer-sqitch deploy --verify db:${dbUrl}"
      )

      primer.systemctl("start podman-primer-service.service")
      primer.wait_for_unit("podman-primer-service.service")
      primer.wait_for_open_port(${port})

      with subtest("version check"):
          primer.succeed("primer-version-check")
    '';

  # Don't wait forever in the event of a problem.
  meta.timeout = 600;
}