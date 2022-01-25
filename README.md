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
nix run .#create-postgresql-container
nix run .#run-postgresql-container
nix run .#create-local-db
```

In general, you should only need to run that sequence the first time
you do any Primer development on a new development machine, or if you
want to start over with a completely new PostgreSQL container for some
reason. The container & database those commands create will persist
across reboots, and will remain on your system until you delete them.

Your usual Primer development workflow will look something like this:

```sh
nix run .#run-postgresql-container
nix run .#run-primer
```

Note that the `run-postgresql-container` is only needed if the
`primer-postgres` container is not already running. Typically, this
will only happen after a reboot, or if you've manually stopped the
container. Therefore, most of the time, you can just do the `nix run
.#run-primer` step.

The details of each script in this repo follow:

### `create-postgresql-container`

This script does the following:

* Uses [colima](https://github.com/abiosoft/colima) to configure a
  Docker-compatible Linux container runtime on your system.
* Downloads the official PostgreSQL Docker image for the version of
  PostgreSQL that we support.
* Creates a persistent Docker volume named `postgres-primer` to ensure
  the database is preserved across container restarts and upgrades.

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

(These scripts do, however, assume that localhost port 5432 is
available for forwarding to the Primer PostgreSQL Docker container.)

### `run-postgresql-container`

This script runs the `primer-postgres` container. The container will
keep running until you reboot your host machine, or you stop the
container yourself.

### `create-local-db`

This script creates the `primer` database in the local PostgreSQL
instance. This database must have been created before Primer can
connect to it. However, Primer is responsible for creating the schema
in the database, and it will do this automatically if the database is
initially empty.

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

# Generating Axios bindings

We can automatically generate TypeScript Axios bindings for
`primer-service`:

```sh
make axios-bindings
```

This will place the generated bindings in the `axios-bindings`
subdirectory. (Note: do not check these bindings into this
repository.)
