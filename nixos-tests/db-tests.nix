{ testingPython
, primer-sqitch
, primer-pgtap-tests
, pg_prove
, ...
}:
with testingPython;
makeTest {
  name = "db-tests";

  meta = with pkgs.lib.maintainers; { maintainers = [ dhess ]; };

  nodes = {
    server = { pkgs, config, ... }: {
      services.postgresql = {
        enable = true;
        package = pkgs.postgresql_13;

        extraPlugins = [ pkgs.postgresql13Packages.pgtap ];

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
        "${pkgs.sudo}/bin/sudo -u primer ${primer-sqitch}/bin/primer-sqitch deploy --verify db:pg:primer"
      )

      server.succeed(
        "${pkgs.sudo}/bin/sudo -u primer ${pg_prove}/bin/pg_prove -v -d primer --ext .sql ${primer-pgtap-tests}/libexec/pgtap/test/"
      )
    '';
}
