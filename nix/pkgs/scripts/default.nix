{ stdenv
, lib
, version
, makeWrapper
, writeShellApplication
, sqitch
, coreutils
, sqitchDir
, sqlite
, primer-service
}:

let
  # Run any sqitch command using the Primer schema.
  primer-sqitch = stdenv.mkDerivation {
    pname = "primer-sqitch";
    version = "1.0";
    nativeBuildInputs = [ sqitch makeWrapper ];
    src = sqitchDir;

    buildPhase = ''
    '';

    installPhase =
      let
        sqitchConfDir = "$out/libexec/sqitch";
      in
      ''
        mkdir -p $out/libexec/sqitch
        sqitch bundle --all --dir $out/libexec/sqitch

        # Rewrite top_dir's so they're absolute paths.
        substituteInPlace $out/libexec/sqitch/sqitch.conf \
          --replace "top_dir = " "top_dir = $out/libexec/sqitch/"

        mkdir -p $out/bin
        makeWrapper "${sqitch}/bin/sqitch" "$out/bin/primer-sqitch" \
          --prefix PATH : "${lib.makeBinPath [sqlite]}" \
          --set SQITCH_CONFIG "$out/libexec/sqitch/sqitch.conf"
      '';
  };

  # The entrypoint for `primer-service` containers. See the shell
  # script source for details.
  primer-service-entrypoint = writeShellApplication {
    name = "primer-service-entrypoint";
    runtimeInputs = [
      coreutils
      primer-service
      primer-sqitch
    ];
    # Use `builtins.readFile` here so that we get a shellcheck.
    text = builtins.readFile ./primer-service-entrypoint.sh;
  };

  # Run `primer-service` locally against a SQLite database. This
  # script sets the expected environment variables, deploys the
  # database, and execs `primer-service-entrypoint`.
  #
  # Note that this script does not need to perform a database
  # deployment before running the entrypoint, because the entrypoint
  # does that for us when running against a SQLite database.
  run-primer-sqlite = writeShellApplication {
    name = "run-primer-sqlite";
    runtimeInputs = [
      primer-sqitch
      primer-service-entrypoint
    ];
    text = ''
      export SERVICE_PORT="''${SERVICE_PORT:-${toString lib.primer.defaultServicePort}}"
      export PRIMER_VERSION="''${PRIMER_VERSION:-${version}}"
      if [ -z ''${SQLITE_DB+x} ]; then
        export SQLITE_DB="primer.sqlite3"
      fi

      primer-service-entrypoint
    '';
  };
in
{
  inherit primer-sqitch;
  inherit primer-service-entrypoint;
  inherit run-primer-sqlite;
}
