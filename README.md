[![Build status](https://badge.buildkite.com/210c4de83d47809721c2b4269b1d00393f3fa24c202abd4d45.svg?branch=main)](https://buildkite.com/hackworthltd/primer-ci)
[![Benchmarks](https://github.com/hackworthltd/primer/actions/workflows/benchmark-action.yaml/badge.svg?branch=main)](https://benchmarks.primer.dev)

# Primer

Primer is a pedagogical functional programming language. This
repository contains Haskell implementations of:

* the core language;

* database adapters for storing Primer programs in a relational
  database; and

* a web-based frontend for editing and running Primer programs.

All projects are licensed under the terms of [version 3 (or later) of
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

* The current implementation of the step evaluator is very slow.
  Running very large programs, or even small programs that generate
  many reduction steps, is not practical at this time. Primer now also
  features an interpreter which is up to a few orders of magnitude
  faster than the step evaluator, but we lose the ability to step
  through programs redex-at-a-time as we can with the step evaluator.
  We plan to do future research to attempt to combine the two
  approaches somehow.

* The Haskell API and database schema are all very unstable, and
  guaranteed to change in compatibility-breaking ways before we reach
  a 1.0 project milestone.

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

# Third-party licenses

Some third-party assets that we distribute together with this
project's source code (e.g., open source fonts) are licensed
separately from the source code. For each such asset, there's a
corresponding license file in the `licenses` subdirectory of the
project.

We have also "vendored" some source code from third-party libraries;
i.e., we have included a few third-party source code files directly in
this project, rather than linking to them as a pre-built dependency.
When this is the case, the source files will include their license and
copyright notice directly in the source file.
