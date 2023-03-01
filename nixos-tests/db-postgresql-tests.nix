{ hostPkgs
, ...
}:
{
  nodes = {
    server = { pkgs, config, ... }: {
      services.postgresql = {
        enable = true;
        package = pkgs.postgresql;

        extraPlugins = [ pkgs.postgresqlPackages.pgtap ];

        # Note: this may look odd, but keep in mind that Sqitch does
        # not create the database, only its schema. In a hosted
        # PostgreSQL service (Fly.io, Google Cloud SQL, AWS Aurora,
        # etc.), the database would be created out-of-band.
        ensureDatabases = [ "primer" ];
        ensureUsers = [
          {
            name = "primer";
            ensurePermissions = {
              "DATABASE primer" = "ALL PRIVILEGES";
            };
          }
        ];
      };

      # This is essential, or else Sqitch will fail.
      time.timeZone = "UTC";

      users.users.primer =
        {
          name = "primer";
          group = "nobody";
          description = "Primer PostgreSQL user";
          isSystemUser = true;
        };

      environment.systemPackages = with pkgs; [
        primer-sqitch
        primer-pg-prove
      ];
    };
  };

  testScript = { nodes, ... }:
    let
      pkgs = nodes.server.pkgs;
    in
    ''
      start_all()
      server.wait_for_unit("postgresql")
      server.succeed(
        "sudo -u primer primer-sqitch deploy --verify db:pg:primer"
      )
      server.succeed(
        "sudo -u primer primer-pg-prove"
      )
    '';
}
