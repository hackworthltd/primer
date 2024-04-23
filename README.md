[![Build status](https://badge.buildkite.com/210c4de83d47809721c2b4269b1d00393f3fa24c202abd4d45.svg?branch=main)](https://buildkite.com/hackworthltd/primer-ci)
[![Benchmarks](https://github.com/hackworthltd/primer/actions/workflows/benchmark-action.yaml/badge.svg?branch=main)](https://benchmarks.primer.dev)

# Primer

Primer is a pedagogical functional programming language. This
repository contains Haskell implementations of:

* the core language;

* database adapters for storing Primer programs in a relational
  database; and

* a web service, or *backend*, which exposes a comprehensive
  programming language API to a client application, or *frontend*.

A companion repository,
[`primer-app`](https://github.com/hackworthltd/primer-app), contains a
web-based frontend application for reading, writing, running, and
debugging Primer programs.

Both projects are licensed under the terms of [version 3 (or later) of
the GNU Affero General Public License](COPYING), and can be freely
copied, modified, and distributed, so long as the license is
preserved.

# Important caveats

Both this implementation of the Primer programming language and the
language specification itself are still under heavy development. If
you use this implementation, you should expect lots of breaking
changes until we reach a [1.0
release](https://github.com/hackworthltd/primer/milestone/4).

For a very rough roadmap, see our [project
milestones](https://github.com/hackworthltd/primer/milestones). Please
note that we don't currently have any accurate time estimates for when
we expect these milestones to be reached, and the milestones are
subject to change.

**Please also read the following important caveats about the current state of the project**:

* There is no authentication (authn) system yet, nor an authorization
  (authz) system, so we strongly recommend that you not host a public
  Primer service until one or both of those systems are in place. It's
  safe to experiment with on a local system, or behind a robust
  identity-aware authentication proxy with a trusted audience, but if
  you host it publicly, be aware that anyone can read, modify, and
  delete any existing program; create new programs; and potentially
  [DoS](https://en.wikipedia.org/wiki/Denial-of-service_attack) the
  server on which it runs.

* The current implementation of the step evaluator is very slow.
  Running very large programs, or even small programs that generate
  many reduction steps, is not practical at this time. Primer now also
  features an interpreter which is up to a few orders of magnitude
  faster than the step evaluator, but we lose the ability to step
  through programs redex-at-a-time as we can with the step evaluator.
  We plan to do future research to attempt to combine the two
  approaches somehow.

* Even the most minor frontend requests result in the entire program
  AST being sent over the network, rather than only what's changed.
  This is an obvious area for improvement, and we're certain that we
  can drastically reduce the amount of data being sent to the client
  application &mdash; we just haven't had a chance to prioritize this
  work yet. For now, however, please keep a very close eye on your
  network traffic if you're communicating with the Primer language
  server over a metered network connection, as the traffic can quickly
  add up.

* The Haskell API, HTTP APIs, and database schema are all very
  unstable, and guaranteed to change in compatibility-breaking ways
  before we reach a 1.0 project milestone.

* The language's semantics are still not fully baked, and there may be
  defects in its design and/or implementation, so existing Primer
  programs will probably break from time to time, with no guaranteed
  forward/backward compatibility path. In other words, you may lose
  work due to incompatibilities between old Primer programs and new
  releases of the language. Once we reach version 1.0, we'll only make
  breaking changes when absolutely necessary, and do our best to
  provide backward compatibility with existing programs, and/or ways
  to migrate old programs to the new semantics.

* At the moment, Primer is a pure functional programming language,
  with no effects system. This means, for example, it's not currently
  possible to [draw animated cats](https://scratch.mit.edu) using
  Primer. We're currently undecided on whether to add an effects
  system to version 2.0 of the language, or to eschew language-level
  effects entirely in favor of a different approach more suited to
  novices.
  
* We've yet to do any rigorous testing of Primer with students, and
  none at all in classrooms. Primer may turn out not be an effective
  way to teach functional programming to novices! That said, if you're
  an educator and you'd like to know more about the limited testing we
  *have* done to date, please reach out to us.

# Contributing

We welcome contributions from the community! Please read our
[contributing guide](CONTRIBUTING.md) if you think you'd like to help.

We also provide a comprehensive [development
guide](docs/development-guide-toc.md) for anyone who'd like to build
and run the project locally.
