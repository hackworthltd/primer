# Database ops

We use [Sqitch](https://sqitch.org/about/) to manage our database
schemas, both for PostgreSQL and SQLite. Because these database
engines are quite different, we require separate schemas for each. For
general help on how to use Sqitch and a bit about how it works, see
[the `sqitch` PostgreSQL
tutorial](https://sqitch.org/docs/manual/sqitchtutorial/).

Note: we do not use Sqitch for any PostgreSQL user or group
permissions, nor any database-wide security settings in general. The
reason for this is because we assume that for some
PostgreSQL-compatible cloud database offerings, user provisioning and
security settings may be managed out of band using the cloud
provider's own APIs. Therefore, our Sqitch scripts assume that the
PostgreSQL user who's running the scripts has all the permissions
required to perform any operations included in those scripts, and that
a [secure schema usage
pattern](https://www.postgresql.org/docs/current/ddl-schemas.html#DDL-SCHEMAS-PATTERNS)
has been configured out-of-band.

The following guide assumes you're running `sqitch` and the
`primer-sqitch` helper script from the `nix develop` shell.

## Sqitch setup

Before running any Sqitch commands locally, you need to configure it
with your name and email address. Note that you need only do this once
per machine where you run Sqitch:

```sh
sqitch config --user user.name "Your Name"
sqitch config --user user.email your-email-address@hackworthltd.com
```

## Schema changes

Making schema changes to the Primer Sqitch configuration is out of
scope for this document. See the [Sqitch documentation for multiple
engines](https://sqitch.org/docs/manual/sqitch-configuration/#separate-plans)
for guidance, and note that we use the "separate plans" approach
described therein. Any Sqitch commands that modify the schema should
be run in the `sqitch` subdirectory of this repo.

## A note about the provided Sqitch scripts

We provide a few Sqitch scripts for common Sqitch operations, in
addition to a `primer-sqitch` wrapper script that bundles a copy of
the project's Sqitch config. We recommend that you at least use the
`primer-sqitch` command rather than running raw `sqitch` when working
with the Primer database. These commands are available in the `nix develop`
shell, and can be run as Flake apps, as well; e.g., `nix run .#primer-sqitch ...`

However, one drawback of this Nix-based approach is that if you're
making local changes to the Sqitch schema, you'll need to make sure
that the scripts in your Nix shell incorporate your changes. This
won't happen automatically in most cases, so either you'll need to
exit and re-enter the Nix shell again; or, if you're using `direnv`
with Nix flake support, run `touch flake.nix`.
