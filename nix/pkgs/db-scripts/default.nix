{ stdenv
, lib
, makeWrapper
, writeShellApplication
, postgresql
, sqitch
, coreutils
, colima
, docker
, sqitchDir
}:

let
  dockerContext = "colima-primer";
  postgresImageTag = "postgres:13.4-alpine3.14";
  postgresVolume = "postgres-primer";
  postgresContainer = "postgres-primer";

  sqitchBundle = stdenv.mkDerivation {
    pname = "bundle-sqitch";
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
      makeWrapper "${sqitch}/bin/sqitch" "$out/bin/sqitch" \
        --prefix PATH : "${lib.makeBinPath [postgresql]}" \
        --run "cd $out/libexec/sqitch"
    '';
  };
in
{
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
      sqitchBundle
    ];
    text = ''
      sqitch deploy --verify db:${lib.primer.postgres-dev-primer-url}
    '';
  };

  verify-local-db = writeShellApplication {
    name = "verify-local-db";
    runtimeInputs = [
      sqitchBundle
    ];
    text = ''
      sqitch verify db:${lib.primer.postgres-dev-primer-url}
    '';
  };

  revert-local-db = writeShellApplication {
    name = "revert-local-db";
    runtimeInputs = [
      sqitchBundle
    ];
    text = ''
      sqitch revert db:${lib.primer.postgres-dev-primer-url} "$@"
    '';
  };

  status-local-db = writeShellApplication {
    name = "status-local-db";
    runtimeInputs = [
      sqitchBundle
    ];
    text = ''
      sqitch status db:${lib.primer.postgres-dev-primer-url}
    '';
  };

  log-local-db = writeShellApplication {
    name = "log-local-db";
    runtimeInputs = [
      sqitchBundle
    ];
    text = ''
      sqitch log db:${lib.primer.postgres-dev-primer-url}
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

  # Run any sqitch command using the Primer schema.
  primer-sqitch = writeShellApplication {
    name = "primer-sqitch";
    runtimeInputs = [
      sqitchBundle
    ];
    text = ''
      sqitch "$@"
    '';
  };
}
