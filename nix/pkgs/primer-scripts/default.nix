# Scripts for running a local PostgreSQL container in
# Docker, and a local primer-service instance.

{ stdenv
, lib
, makeWrapper
, writeShellApplication
, postgresql
, sqitch
, coreutils
, primer-service
, colima
, docker
, primerVersion
, sqitchDir
}:

let

  dockerContext = "colima-primer";
  postgresImageTag = "postgres:13.4-alpine3.14";
  postgresVolume = "postgres-primer";
  postgresContainer = "postgres-primer";
  postgresPassword = "primer-dev";
  postgresBaseUrl = "postgres://postgres:${postgresPassword}@localhost:5432";
  postgresPrimerUrl = "${postgresBaseUrl}/primer";

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
      docker --context ${dockerContext} run --detach --name=${postgresContainer} --publish 5432:5432 --volume ${postgresVolume}:/var/lib/postgresql/data -e POSTGRES_PASSWORD="${postgresPassword}" ${postgresImageTag}
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
      psql ${postgresBaseUrl} --command="CREATE DATABASE primer;"
    '';
  };

  deploy-local-db = writeShellApplication {
    name = "deploy-local-db";
    runtimeInputs = [
      sqitchBundle
    ];
    text = ''
      sqitch deploy --verify db:${postgresPrimerUrl}
    '';
  };

  verify-local-db = writeShellApplication {
    name = "verify-local-db";
    runtimeInputs = [
      sqitchBundle
    ];
    text = ''
      sqitch verify db:${postgresPrimerUrl}
    '';
  };

  revert-local-db = writeShellApplication {
    name = "revert-local-db";
    runtimeInputs = [
      sqitchBundle
    ];
    text = ''
      sqitch revert db:${postgresPrimerUrl} "$@"
    '';
  };

  status-local-db = writeShellApplication {
    name = "status-local-db";
    runtimeInputs = [
      sqitchBundle
    ];
    text = ''
      sqitch status db:${postgresPrimerUrl}
    '';
  };

  log-local-db = writeShellApplication {
    name = "log-local-db";
    runtimeInputs = [
      sqitchBundle
    ];
    text = ''
      sqitch log db:${postgresPrimerUrl}
    '';
  };

  delete-local-db = writeShellApplication {
    name = "delete-local-db";
    runtimeInputs = [
      postgresql
    ];
    text = ''
      psql ${postgresBaseUrl} --command="DROP DATABASE primer;"
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
      pg_dump ${postgresPrimerUrl} > "$dumpfile"
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
      psql ${postgresBaseUrl} --command="DROP DATABASE primer;" || true
      psql ${postgresBaseUrl} --command="CREATE DATABASE primer;"
      psql ${postgresPrimerUrl} < "$1"
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

  run-primer = writeShellApplication {
    name = "run-primer";
    runtimeInputs = [
      primer-service
    ];
    text = ''
      DATABASE_URL="${postgresPrimerUrl}"
      export DATABASE_URL
      primer-service serve . ${primerVersion} "$@"
    '';
  };


  ## Database unit/integration tests.
  primer-init-db-test = writeShellApplication
    {
      name = "primer-init-db-test";
      runtimeInputs = [
        sqitchBundle
      ];
      text = ''
        DATABASE_URL="postgres://postgres:$POSTGRES_PASSWORD@$DOCKER_SERVICE_IP:$LOCAL_POSTGRES_PORT"
        export DATABASE_URL
        sqitch deploy --verify db:"$DATABASE_URL"
      '';
    };
}
