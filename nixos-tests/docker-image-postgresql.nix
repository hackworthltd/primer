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

  altPort = toString (hostPkgs.lib.primer.defaultServicePort + 1);
  altVersion = "alt-primer";
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
            PORT="$1"
            VERSION="$2"
            RESULT=$(curl http://localhost:"$PORT"/api/version | jq -r)
            if [ "$RESULT" != "$VERSION" ]; then
              echo "Expected primer-service version $VERSION, but got $RESULT" >& 2
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
          diskSize = 3072;
          memorySize = 1024;
        };

        virtualisation.oci-containers = {
          containers.primer-service = {
            image = "primer-service:${pkgs.primer-service-docker-image.imageTag}";
            imageFile = pkgs.primer-service-docker-image;

            ports = [ "${port}:${port}" ];
            extraOptions = [ "--network=host" ];
            environment = {
              DATABASE_URL = dbUrl;
            };
          };

          # Ensure we can override the default PRIMER_VERSION and
          # SERVICE_PORT environment variables.
          containers.primer-service-alt = {
            image = "primer-service:${pkgs.primer-service-docker-image.imageTag}";
            imageFile = pkgs.primer-service-docker-image;

            ports = [ "${altPort}:${altPort}" ];
            extraOptions = [ "--network=host" ];
            environment = {
              DATABASE_URL = dbUrl;
              SERVICE_PORT = toString altPort;
              PRIMER_VERSION = altVersion;
            };
          };
        };

        # If the container doesn't start cleanly, the test has failed.
        systemd.services.podman-primer-service.serviceConfig.Restart = pkgs.lib.mkForce "no";
        systemd.services.podman-primer-service-alt.serviceConfig.Restart = pkgs.lib.mkForce "no";

        # Make sure we can see container failures.
        systemd.services.podman-primer-service.serviceConfig.StandardOutput = pkgs.lib.mkForce "journal";
        systemd.services.podman-primer-service.serviceConfig.StandardError = pkgs.lib.mkForce "journal";
        systemd.services.podman-primer-service-alt.serviceConfig.StandardOutput = pkgs.lib.mkForce "journal";
        systemd.services.podman-primer-service-alt.serviceConfig.StandardError = pkgs.lib.mkForce "journal";

        # We want to manually start and stop the container.
        systemd.services.podman-primer-service.wantedBy = pkgs.lib.mkForce [ ];
        systemd.services.podman-primer-service-alt.wantedBy = pkgs.lib.mkForce [ ];

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
          primer.succeed("primer-version-check ${port} ${version}")

      primer.systemctl("start podman-primer-service-alt.service")
      primer.wait_for_unit("podman-primer-service-alt.service")
      primer.wait_for_open_port(${altPort})

      with subtest("alt version check"):
          primer.succeed("primer-version-check ${altPort} ${altVersion}")
    '';

  # Don't wait forever in the event of a problem.
  meta.timeout = 600;
}
