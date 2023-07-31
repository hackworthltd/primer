USAGE=$(cat <<EOF
This is the entrypoint script for primer-service containers. It first
uses sqitch to verify that the provided database uses the expected
schema. If not, it fails; otherwise, it starts primer-service.

This script accepts no command-line arguments, and will fail if any
are provided. All options to primer-service are provided via specific
environment variables:

- SQLITE_DB: A path to a SQLite database file.

- CORS_ALLOW_ORIGIN: A comma-separated list of one or more CORS
  origins to allow; e.g.,
  "https://app.example.com,https://dev.example.com". Each origin in
  the list must be formatted per RFC 6454. (Note that the service does
  not use a validating parser, and may exhibit unexpected behavior if
  one or more origins aren't correctly formatted.) The origins "*" and
  "null" are not permitted. If this variable isn't set, then the
  service will respond to CORS preflight requests with the wildcard
  ("*") origin, which effectively precludes any possibility of using
  the service behind an authenticating proxy.

- SERVICE_PORT: A TCP port number on which the Primer service
  listens for HTTP connections. This variable is required.

- PRIMER_VERSION: The version reported by Primer's API. This
  variable is required.
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

if [ -z "${SQLITE_DB+x}" ]; then
    echo "SQLITE_DB is not set, exiting." >&2
    exit 4
else
    # Ensure any required database migrations are performed before
    # starting the service. This is safe in the case of a SQLite
    # target, because we rely on the container orchestrator to
    # guarantee that we are the only instance that has mounted
    # the volume containing the database.
    SQITCH_TARGET="db:sqlite:$SQLITE_DB" primer-sqitch deploy --verify
fi

if [ -n "${CORS_ALLOW_ORIGIN+x}" ]; then
    EXTRA_PRIMER_SERVICE_ARGS="$EXTRA_PRIMER_SERVICE_ARGS --cors-allow-origin $CORS_ALLOW_ORIGIN"
fi

# shellcheck disable=SC2086
exec primer-service serve "$PRIMER_VERSION" "$SQLITE_DB" --port "$SERVICE_PORT" $EXTRA_PRIMER_SERVICE_ARGS +RTS -T
