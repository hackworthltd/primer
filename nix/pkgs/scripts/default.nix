{ stdenv
, lib
, version
, makeWrapper
, writeShellApplication
, postgresql
, sqitch
, coreutils
, colima
, docker
, gnugrep
, sqitchDir
, primer-service
}:

let
  dockerContext = "colima-primer";
  postgresImageTag = "postgres:13.4-alpine3.14";
  postgresVolume = "postgres-primer";
  postgresContainer = "postgres-primer";

  # Run any sqitch command using the Primer schema.
  primer-sqitch = stdenv.mkDerivation {
    pname = "primer-sqitch";
    version = "1.0";
    nativeBuildInputs = [ sqitch makeWrapper ];
    src = sqitchDir;

    buildPhase = ''
      sqitch bundle
    '';

    installPhase = ''
      mkdir -p $out/libexec
      mv bundle $out/libexec/sqitch

      mkdir -p $out/bin
      makeWrapper "${sqitch}/bin/sqitch" "$out/bin/primer-sqitch" \
        --prefix PATH : "${lib.makeBinPath [postgresql]}" \
        --run "cd $out/libexec/sqitch"
    '';
  };

  # Bundle our PostgreSQL unit tests, to be used via `pgtap`/`pg_prove`.
  primer-pgtap-tests = stdenv.mkDerivation {
    pname = "primer-pgtap-tests";
    version = "1.0";
    src = "${sqitchDir}/test";

    buildPhase = "";

    installPhase = ''
      mkdir -p $out/libexec/pgtap/test
      mv * $out/libexec/pgtap/test
    '';
  };
in
{
  inherit primer-sqitch;
  inherit primer-pgtap-tests;

  deploy-postgresql-container = writeShellApplication {
    name = "deploy-postgresql-container";
    runtimeInputs = [
      colima
      docker
    ];
    text = ''
      colima start --runtime docker --profile primer
      docker --context ${dockerContext} pull ${postgresImageTag}
      docker volume create postgres-primer
      docker --context ${dockerContext} run --detach --name=${postgresContainer} --publish 5432:5432 --volume ${postgresVolume}:/var/lib/postgresql/data -e POSTGRES_PASSWORD="${lib.primer.postgres-dev-password}" ${postgresImageTag}
    '';
  };

  start-postgresql-container = writeShellApplication {
    name = "start-postgresql-container";
    runtimeInputs = [
      docker
    ];
    text = ''
      docker --context ${dockerContext} start ${postgresContainer}
    '';
  };

  stop-postgresql-container = writeShellApplication {
    name = "stop-postgresql-container";
    runtimeInputs = [
      docker
    ];
    text = ''
      docker --context ${dockerContext} stop ${postgresContainer}
    '';
  };

  create-local-db = writeShellApplication {
    name = "create-local-db";
    runtimeInputs = [
      postgresql
    ];
    text = ''
      psql ${lib.primer.postgres-dev-base-url} --command="CREATE DATABASE primer;"
    '';
  };

  deploy-local-db = writeShellApplication {
    name = "deploy-local-db";
    runtimeInputs = [
      primer-sqitch
    ];
    text = ''
      primer-sqitch deploy --verify db:${lib.primer.postgres-dev-primer-url}
    '';
  };

  verify-local-db = writeShellApplication {
    name = "verify-local-db";
    runtimeInputs = [
      primer-sqitch
    ];
    text = ''
      primer-sqitch verify db:${lib.primer.postgres-dev-primer-url}
    '';
  };

  revert-local-db = writeShellApplication {
    name = "revert-local-db";
    runtimeInputs = [
      primer-sqitch
    ];
    text = ''
      primer-sqitch revert db:${lib.primer.postgres-dev-primer-url} "$@"
    '';
  };

  status-local-db = writeShellApplication {
    name = "status-local-db";
    runtimeInputs = [
      primer-sqitch
    ];
    text = ''
      primer-sqitch status db:${lib.primer.postgres-dev-primer-url}
    '';
  };

  log-local-db = writeShellApplication {
    name = "log-local-db";
    runtimeInputs = [
      primer-sqitch
    ];
    text = ''
      primer-sqitch log db:${lib.primer.postgres-dev-primer-url}
    '';
  };

  delete-local-db = writeShellApplication {
    name = "delete-local-db";
    runtimeInputs = [
      postgresql
    ];
    text = ''
      psql ${lib.primer.postgres-dev-base-url} --command="DROP DATABASE primer;"
    '';
  };

  dump-local-db = writeShellApplication {
    name = "dump-local-db";
    runtimeInputs = [
      coreutils
      postgresql
    ];
    text = ''
      timestamp=$(date --utc --iso-8601=seconds)
      dumpfile="primer-$timestamp.sql"
      pg_dump ${lib.primer.postgres-dev-primer-url} > "$dumpfile"
      echo "Dumped local Primer database to $dumpfile"
    '';
  };

  restore-local-db = writeShellApplication {
    name = "restore-local-db";
    runtimeInputs = [
      postgresql
    ];
    text = ''
      if [[ $# -ne 1 ]]; then
        echo "usage: restore-local-db db.sql" >&2
        exit 2
      fi
      psql ${lib.primer.postgres-dev-base-url} --command="DROP DATABASE primer;" || true
      psql ${lib.primer.postgres-dev-base-url} --command="CREATE DATABASE primer;"
      psql ${lib.primer.postgres-dev-primer-url} < "$1"
    '';
  };

  primer-service-entrypoint = writeShellApplication {
    name = "primer-service-entrypoint";
    runtimeInputs = [
      primer-service
      primer-sqitch
    ];
    text = ''
      if [ -z ''${DATABASE_URL+x} ]; then
        echo "DATABASE_URL is not set, exiting." >&2
        exit 1
      fi
      primer-sqitch verify db:"$DATABASE_URL"
      exec primer-service serve . "${version}" --port ${toString lib.primer.defaultServicePort}
    '';
  };
}
