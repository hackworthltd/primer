[![Build status](https://badge.buildkite.com/7a9fd1213265a375385deec0a418bc4a6f26b17b7f4efe3cad.svg?branch=main)](https://buildkite.com/hackworthltd/primer)
[![Benchmarks](https://github.com/hackworthltd/primer/actions/workflows/benchmark-action.yaml/badge.svg?branch=main)](https://hackworthltd.github.io/primer/dev/bench/)

# Primer

Primer is a pedagogical functional programming language. This
repository contains Haskell implementations of:

* the core language;
* a database adapter for storing Primer programs; and
* a web service.

Note that Primer must be built with GHC 9.2.1 or later.

We currently support two SQL backend implementations: SQLite and
PostgreSQL.

# Developing Primer

## Nix

We use [Nix flakes](https://nixos.wiki/wiki/Flakes) to develop Primer,
so most of the documentation that follows assumes you've got a working
Nix installation with flakes support enabled. Our flake supports both
`x86_64-linux` and `aarch64-darwin` systems.

## Cabal

Primer itself is a standard Haskell Cabal project. We recommend that
you develop in the `nix develop` shell, in which case Nix will provide
supported versions of GHC, Cabal, and any other tooling required to
build the project.

You *can* choose to ignore Nix and build Primer's libraries and
executables without it, assuming you have supported versions of GHC
and Cabal in your `PATH`. However, in order to run some of Primer's
tests, you'll need additional bespoke tools that are provided
automatically by the `nix develop` shell (e.g., `primer-sqitch`).

### `make` targets

For interactive development workflows, we provide some convenient
`make` targets from the repository's top level directory:

* `make` runs `cabal configure` followed by `cabal build` across all
  projects.

* `make test` runs `cabal test` across all projects.

* `make bench` runs `cabal bench` across all projects.

Because running the API server typically involves a few different
moving parts, we do not provide `make` targets for running the service
locally, and instead recommend that you use either `nix run
.#run-primer-sqlite` or `nix run .#run-primer-postgresql` as described
below.

## Local development with SQLite

Developing Primer with a local SQLite database is straightforward, so
long as you have a working Nix flakes setup. Just run the following
command:

```sh
nix run .#run-primer-sqlite
```

By default, this command will:

1. Deploy a SQLite database named `primer.sqlite3` in the current
   working directory. It will create a new empty database if one
   doesn't already exist.
2. Run `primer-service` on your host machine and configure it to
   listen on TCP port `8081` on all network interfaces.

This command uses the same script to launch the service as our Docker
container uses, and therefore it takes no command-line arguments.
Instead, you can override the defaults by setting any of the following
environment variables (shown along with their default values):

| Environment variable |      Purpose                                          |  Default         |
|----------------------|-------------------------------------------------------|------------------|
| `SQLITE_DB`          | Filesystem path to the SQLite database                | `primer.sqlite3` |
| `SERVICE_PORT`       | TCP port on which the service listens for connections | `8081`           |

Note that the script will actually create *two* database files: one
named `primer.sqlite3`, and the other named `sqitch.sqlite3`. The
latter contains metadata required by
[Sqitch](https://sqitch.org/docs/manual/sqitchtutorial-sqlite/), which
we use to manage database schemas.

If you want to start over with a new database, either set `SQLITE_DB`
to a new path, or remove both the `primer.sqlite3` and
`sqitch.sqlite3` database files.

If you don't want to use the `run-primer-sqlite` command for some
reason, you can also run `primer-service` directly via a Nix flake
app. For usage, run:

```sh
nix run .#primer-service serve -- --help
```

## Local development with PostgreSQL

Developing Primer with a PostgreSQL database backend is much more
involved than with SQLite, but we do provide some Nix tooling to make
it a bit easier.

### Initial setup

The first time you want to do local Primer development on a particular
host, you'll need to run the following commands from the top-level
directory in this repo:

```sh
nix run .#deploy-postgresql-container
nix run .#create-local-db
```

This sequence of commands will do the following:

1. Create a new [Colima](https://github.com/abiosoft/colima) profile named `primer`.
2. Run a PostgreSQL container in the `primer` profile, using Colima's
   Docker-compatible runtime. The PostgreSQL service running on the
   container listens on the default PostgreSQL TCP port, `5432`.
3. Create a PostgreSQL database named `primer`.

In general, you should only need to run this sequence of commands the
first time you do any Primer development on a new development machine,
or if you want to start over with a completely new PostgreSQL
container for some reason. The container & database those commands
create will persist across reboots, and will remain on your system
until you delete them. However, it's safe to run these commands
multiple times, as they will create only a single container and
`primer` database.

### Development workflow

Your usual Primer development workflow will look something like this:

```sh
nix run .#run-primer-postgresql
```

By default, this command will:

1. Deploy the Primer database schema to the default local PostgreSQL
   instance.
2. Run `primer-service` on your host machine and configure it to
   listen on TCP port `8081` on all network interfaces.

This command uses the same script to launch the service as our Docker
container uses, and therefore it takes no command-line arguments.
Instead, you can override the defaults by setting any of the following
environment variables (shown along with their default values):

| Environment variable |      Purpose                                          |  Default                                               |
|----------------------|-------------------------------------------------------|--------------------------------------------------------|
| `DATABASE_URL`       | The PostgreSQL-style URI of the database              | `postgres://postgres:primer-dev@localhost:5432/primer` |
| `SERVICE_PORT`       | TCP port on which the service listens for connections | `8081`                                                 |

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
nix run .#start-postgresql-container
```

### PostgreSQL helper scripts

The following helper scripts are also available for the PostgreSQL
development workflow. These can be run in the `nix develop` shell, or
as Nix flake apps via `nix run`.

#### `deploy-postgresql-container`

This script does the following:

* Uses Colima to configure a Docker-compatible Linux container runtime
  on your system.
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

#### `start-postgresql-container`

This script starts the `primer-postgres` container, assuming that it's
previously been deployed by the `deploy-postgresql-container` command.
The container will keep running until you reboot your host machine, or
you stop the container yourself.

#### `stop-postgresql-container`

This script stops the `primer-postgres` container.

#### `create-local-db`

This script creates the `primer` database in the local PostgreSQL
instance. This database must have been created before Primer can
connect to it.

#### `deploy-local-db`

This script ensures your local database is using the latest schema.
You'll need to run it anytime there's a schema change. We'll do our
best to broadcast when this is necessary.

Note that this script is safe to run at any time, even if the database
is already using the latest schema.

#### Other helper scripts

You'll probably use these additional scripts less often than the
others, but they're available if you need them:

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
  the following flags: `-- --to <rev>`. For example, to revert any
  changes made since `HEAD`, run `nix run .#revert-local-db -- --to @HEAD`.
