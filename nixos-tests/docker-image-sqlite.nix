{ hostPkgs
, ...
}:
let
  hostDbPath = "/tmp/primer.db";
  dbPath = "/primer/primer.db";
  port = toString hostPkgs.lib.primer.defaultServicePort;
  version = hostPkgs.lib.primer.version;

  altPort = toString (hostPkgs.lib.primer.defaultServicePort + 1);
  altVersion = "alt-primer";
in
{
  nodes = {
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
            volumes = [
              "/tmp:/primer"
            ];
            extraOptions = [ "--network=host" ];
            environment = {
              SQLITE_DB = dbPath;
            };
          };

          # Ensure we can override the default PRIMER_VERSION and
          # SERVICE_PORT environment variables.
          containers.primer-service-alt = {
            image = "primer-service:${pkgs.primer-service-docker-image.imageTag}";
            imageFile = pkgs.primer-service-docker-image;

            ports = [ "${altPort}:${altPort}" ];
            volumes = [
              "/tmp:/primer"
            ];
            extraOptions = [ "--network=host" ];
            environment = {
              SQLITE_DB = dbPath;
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
          primer-sqitch
          versionCheck
        ];
      };
  };

  testScript = { nodes, ... }:
    ''
      primer.start();
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
