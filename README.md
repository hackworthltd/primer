[![Build status](https://badge.buildkite.com/7a9fd1213265a375385deec0a418bc4a6f26b17b7f4efe3cad.svg?branch=main)](https://buildkite.com/hackworthltd/primer)

# Primer

Primer is a pedagogical functional programming language. This
repository contains Haskell implementations of:

* the core language;
* a database adapter for storing Primer programs; and
* a web service.

# Running Primer

## Local development

To run Primer for local development, you'll need a PostgreSQL instance
to develop against. The most straightforward way to do this is via
some scripts that are included in this repo. Each script can be run
via the `nix run .#script-name` command, where `script-name` should be
replaced by one of the scripts described below.

The first time you want to do local Primer development on a particular
system, you'll need to run the following commands from the top-level
directory in this repo:

```sh
nix run .#deploy-postgresql-container
nix run .#create-local-db
nix run .#deploy-local-db
```

In general, you should only need to run that sequence the first time
you do any Primer development on a new development machine, or if you
want to start over with a completely new PostgreSQL container for some
reason. The container & database those commands create will persist
across reboots, and will remain on your system until you delete them.

Your usual Primer development workflow will look something like this:

```sh
nix run .#run-primer
```

Note that you'll also need to run the `start-postgresql-container`
command if the `primer-postgres` container is not already running.
Typically, this will only happen after a reboot, or if you've manually
stopped the container. To determine whether the container is running,
use this command from the project's Nix shell:

```sh
docker --context colima-primer ps
```

If it's running, you should see something like this:

```
CONTAINER ID   IMAGE                      COMMAND                  CREATED        STATUS         PORTS                                       NAMES
a818e6d5f3ef   postgres:13.4-alpine3.14   "docker-entrypoint.s…"   29 hours ago   Up 2 minutes   0.0.0.0:5432->5432/tcp, :::5432->5432/tcp   postgres-primer
```

If it's not, then perform the following steps:

```sh
colima start --runtime docker --profile primer
nix run .#start-posgresql-container
```

The details of each script in this repo follow:

### `deploy-postgresql-container`

This script does the following:

* Uses [colima](https://github.com/abiosoft/colima) to configure a
  Docker-compatible Linux container runtime on your system.
* Downloads the official PostgreSQL Docker image for the version of
  PostgreSQL that we support.
* Creates a persistent Docker volume named `postgres-primer` to ensure
  the database is preserved across container restarts and upgrades.
* Creates a Docker container that runs PostgreSQL and listens on
  `localhost:5432`.

Note that you do *not* need to install or run Docker in order to use
this or any other script in this repo, as Colima provides the required
container functionality. The scripts do use the `docker` command-line
utility, but only to manage the container, images, and persistent
volumes.

If you're already running Docker, Colima works alongside it without
conflict. The scripts in this repo will run all containers in a
separate `colima-primer` Docker context, in order to keep the Primer
development environment from affecting any other Docker contexts you
may be using.

### `start-postgresql-container`

This script starts the `primer-postgres` container, assuming that it's
previously been deployed by the `deploy-postgresql-container` command.
The container will keep running until you reboot your host machine, or
you stop the container yourself.

### `stop-postgresql-container`

This script stops the `primer-postgres` container.

### `create-local-db`

This script creates the `primer` database in the local PostgreSQL
instance. This database must have been created before Primer can
connect to it.

### `deploy-local-db`

This script ensures your local database is using the latest schema.
You'll need to run it anytime there's a schema change. We'll do our
best to broadcast when this is necessary.

Note that this script is safe to run at any time, even if the database
is already using the latest schema.

### `run-primer`

This script runs the Primer service and connects to the local
PostgreSQL database. This is the script you'll run most often while
hacking on Primer.

### Helper scripts

* `delete-local-db` drops the Primer database from the local
  PostgreSQL instance. **Warning**: this script will delete all of the
  Primer programs in your local database.

* `dump-local-db` dumps the Primer database from the local PostgreSQL
  instance. It's mainly useful in combination with the
  `restore-local-db` script.

* `restore-local-db` restores a Primer database dump by dropping the
  existing Primer database (**warning**: the existing database
  contents will not be saved!), creating a fresh, empty Primer
  database, and then loading the dump into the new database. This
  script is most useful in combination with the `dump-local-db`
  script.

* `verify-local-db` reports the differences between the latest schema
  and the schema your local database is using.

* `revert-local-db` reverts the local database to a previous schema.
  Note that this may not always be successful. You should generally
  only need to run this command if you're testing database schema
  migrations. You can specify which git commit to revert to by passing
  the following flags: `-- --to <rev>`.

## A note about the Sqitch scripts

Each Sqitch script bundles a copy of the Sqitch config. This means you
can run these scripts from anywhere, Sqitch configs contribute to the
Nix store hash for their corresponding scripts, etc.

One drawback of this approach is that if you're making local changes
to the schema, you'll need to make sure that the scripts in your Nix
shell incorporate your changes. This won't happen automatically in
most cases, so either you'll need to exit and re-enter the Nix shell
via `nix develop`, or, if you're running `direnv`, you can usually run
`touch flake.nix`.

# Database ops

We use [Sqitch](https://sqitch.org/about/) to manage our database
schema.

Most of what you need to know to use `sqitch` with Primer is included
in this section, but for general help on how to use `sqitch` and a bit
about how it works, see [the
tutorial](https://sqitch.org/docs/manual/sqitchtutorial/).

Note: we do not use Sqitch for any user or group permissions, nor any
database-wide security settings in general. The reason for this is
because we assume that for some PostgreSQL-compatible cloud database
offerings, user provisioning and security settings may be managed out
of band using the cloud provider's own APIs. Therefore, our Sqitch
scripts assume that the PostgreSQL user who's running the scripts has
all the permissions required to perform any operations included in
those scripts, and that a [secure schema usage
pattern](https://www.postgresql.org/docs/current/ddl-schemas.html#DDL-SCHEMAS-PATTERNS)
has been configured out-of-band.


## Sqitch setup

Before running any `sqitch` commands, you need to configure it with
your name and email address. Note that you need only do this once per
machine where you run `sqitch`:

```sh
sqitch config --user user.name "Your Name"
sqitch config --user user.email your-email-address@hackworthltd.com
```

## Add a table

To add a new table to the database:

1. Tell `sqitch` about the new table:

```sh
cd sqitch
sqitch add table_name --requires appschema -n "Description of new table."
```

2. Edit the `sqitch` scripts for the new table. See [the
tutorial](https://sqitch.org/docs/manual/sqitchtutorial/) for details.

Note: we follow the [same convention as the maintainer of
Sqitch](https://github.com/sqitchers/sqitch/issues/239#issuecomment-118943207)
when setting PostgreSQL search paths/schema prefixes in our scripts.

# Generating Axios bindings

We can automatically generate TypeScript Axios bindings for
`primer-service`:

```sh
make axios-bindings
```

This will place the generated bindings in the `axios-bindings`
subdirectory. (Note: do not check these bindings into this
repository.)
