# Developing Primer

## Nix

We use [Nix flakes](https://nixos.wiki/wiki/Flakes) to develop Primer,
so most of our developer documentation assumes you've got a working
Nix installation with flakes support enabled.

Our flake supports both `x86_64-linux` and `aarch64-darwin` systems.
Adding support for other common architectures should be
straightforward, but we don't currently have the CI resources to
support them.

## Cabal

Primer itself is a standard Haskell Cabal project. We recommend that
you work in the provided `nix develop` shell, because then Nix will
provide supported versions of GHC, Cabal, and any other tooling
required to build, test, and run the project.

You *can* choose to forego Nix and build Primer's libraries and
executables without it, assuming you have supported versions of GHC
and Cabal in your `PATH`. However, in order to run some of Primer's
tests, you'll need additional bespoke tools that are provided
automatically by the `nix develop` shell (e.g., `primer-sqitch`).
Please note that we don't have the maintainer resources to support
these tools in non-Nix environments.

## `Makefile` targets

For interactive development workflows, we provide some convenient
`Makefile` targets from the repository's top level directory:

* `make` runs `cabal configure` followed by `cabal build` across all
  projects.

* `make test` runs `cabal test` across all projects.

* `make bench` runs `cabal bench` across all projects.

Because running the API server for local development involves a few
different moving parts, we don't provide `make` targets for running
the service, and instead recommend that you use `nix run` for this
purpose, as described below.

## Local development with SQLite

Running the API server against a local SQLite database is
straightforward, so long as you have a working Nix flakes setup. Just
run the following command from the repo's top-level directory:

```sh
nix run .#run-primer-sqlite
```

By default, this command will:

1. Deploy a SQLite database named `primer.sqlite3` in the current
   working directory. It will create a new empty database if one
   doesn't already exist.
2. Run `primer-service` on your host machine and configure it to
   listen on TCP port `8081` on all network interfaces.

This command uses the same script to launch the service as our
production Docker container uses, and therefore, in typical Docker
entrypoint style, it takes no command-line arguments. Instead, you can
override the server's default configuration by setting any of the
following environment variables (shown along with their default
values):

| Environment variable |      Purpose                                          |  Default         |
|----------------------|-------------------------------------------------------|------------------|
| `SQLITE_DB`          | Filesystem path to the SQLite database                | `primer.sqlite3` |
| `SERVICE_PORT`       | TCP port on which the service listens for connections | `8081`           |

Note that the script will actually create *two* database files: one
named `primer.sqlite3`, and the other named `sqitch.sqlite3`. The
latter contains metadata required by
[Sqitch](https://sqitch.org/docs/manual/sqitchtutorial-sqlite/), which
we use to manage [database schemas](database.md).

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

In addition to SQLite, `primer-service` also supports PostgreSQL for
its program store. Though we recommend using SQLite for local
development due to its much simpler setup, we do provide a collection
of Nix-based tooling to make local PostgreSQL development as easy as
we can. See the [PostgreSQL development documentation](postgresql.md)
for details.
