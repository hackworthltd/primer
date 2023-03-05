USAGE=$(cat <<EOF
This is the entrypoint script for primer-service containers. It first
uses sqitch to verify that the provided database uses the expected
schema. If not, it fails; otherwise, it starts primer-service.

This script accepts no command-line arguments, and will fail if any
are provided. All options to primer-service are provided via specific
environment variables:

- DATABASE_URL: A PostgreSQL-style connection URI. See
  https://www.postgresql.org/docs/14/libpq-connect.html#LIBPQ-CONNSTRING

- SQLITE_DB: A path to a SQLite database file.

- SERVICE_PORT: A TCP port number on which the Primer service
  listens for HTTP connections. This variable is required.

- PRIMER_VERSION: The version reported by Primer's API. This
  variable is required.

Note that exactly one of DATABASE_URL or SQLITE_DB must be set,
otherwise the entrypoint script will fail; i.e., these two variables
are mutually exclusive.
EOF
)

usage () {
    program=$(basename "$0")
    echo "Usage: $program" >&2
    echo >&2
    echo "$USAGE" >&2
}

if [ "$#" -ne 0 ]; then
    usage
    exit 1
fi

if [ -z "${SERVICE_PORT+x}" ]; then
    echo "SERVICE_PORT is not set, exiting." >&2
    exit 2
fi

if [ -z "${PRIMER_VERSION+x}" ]; then
    echo "PRIMER_VERSION is not set, exiting." >&2
    exit 3
fi

EXTRA_PRIMER_SERVICE_ARGS=""

if [ -z "${DATABASE_URL+x}" ]; then

    # DATABASE_URL is not set.

    if [ -z "${SQLITE_DB+x}" ]; then
        echo "Neither DATABASE_URL nor SQLITE_DB is set, exiting." >&2
        exit 4
    else
        EXTRA_PRIMER_SERVICE_ARGS="--sqlite-db $SQLITE_DB"

        # Ensure any required database migrations are performed before
        # starting the service. This is safe in the case of a SQLite
        # target, because we rely on the container orchestrator to
        # guarantee that we are the only instance that has mounted
        # the volume containing the database.
        SQITCH_TARGET="db:sqlite:$SQLITE_DB" primer-sqitch deploy --verify
    fi

else

    # DATABASE_URL is set.
    #
    # Note that DATABASE_URL probably contains a secret, so don't
    # convert it to a command-line argument. We use the SQITCH_TARGET
    # env var for sqitch, and primer-service will look for
    # DATABASE_URL of its own accord.

    if [ -z "${SQLITE_DB+x}" ]; then
        # Note: we do not deploy when the target is PostgreSQL, only
        # verify. This is because concurrent migrations are not safe,
        # and we cannot guarantee that we are the only container that
        # is starting.
        #
        # See:
        # https://github.com/sqitchers/sqitch/discussions/646
        SQITCH_TARGET="db:$DATABASE_URL" primer-sqitch verify
    else
        echo "Both DATABASE_URL and SQLITE_DB are set, but you must provide only one. Exiting." >&2
        exit 5
    fi
fi



# shellcheck disable=SC2086
exec primer-service serve "$PRIMER_VERSION" --port "$SERVICE_PORT" $EXTRA_PRIMER_SERVICE_ARGS +RTS -T
