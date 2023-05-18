# Project overview

Our implementation of the Primer programming language is not like most
other programming language implementations. We don't provide a typical
command-line compiler, or `readline`-based REPL/interpreter, for
example.

## Programming language as a service

Instead, our implementation of the Primer programming language is
intended for use as a sort of über language server, but not in the
more familiar sense of the [Microsoft Language Server
Protocol](https://microsoft.github.io/language-server-protocol/)
(LSP). Unlike LSP servers, the Primer language server is the single
source of truth for a given Primer program. The Primer language server
provides the following services to its clients:

* It stores and retrieves programs from a program store, backed by a
  database engine. In our implementation of Primer, there's no concept
  of programs stored in flat text files in a hierarchical filesystem.

* It provides the program's abstract syntax tree (AST) to the client,
  along with a list of type-safe, atomic AST editing actions that can
  be performed at the currently-selected node, making it relatively
  easy to implement structure editors.

* It type checks the entire program, and can automatically fix up
  programs after editing actions are performed, guaranteeing that
  programs are always well-formed, modulo holes (which are similar in
  concept to GHC's [typed
  holes](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/typed_holes.html?highlight=typed+holes),
  but implemented quite differently).

* It provides both a small-step reducible expression (*redex*)
  evaluator, for tracing program evaluation in learning and debugging
  contexts, and a "big-step" evaluator, for running programs to
  (bounded) termination.

In a nutshell, the Primer language server provides all of the
functionality that one needs to read, write, run, debug, and store
programs via a single, comprehensive API. Developers of Primer
programming environments can focus on writing fluid, cohesive UIs and
delivering a great user experience (UX), without getting bogged down
in gluing together a number of disparate tools, each parsing and
analyzing the same program over and over in inefficient, often brittle
ways.

In its current implementation, the Primer language server runs as a
native executable, typically in a containerized Linux environment, but
in the not-too-distant future, we expect to generate a JavaScript
and/or WebAssembly executable that can run most of these services
directly in the browser, eliminating the most expensive and least
scalable aspects of the backend service.

## Project organization

This Primer project is a collection of Haskell packages, which
together implement a Primer language service:

* `primer` implements the Primer language, including the type checker,
  the two evaluators, and an API that exposes these features to other
  Haskell programs.

* `primer-rel8` and `primer-selda` implement, respectively,
  [PostgreSQL](https://www.postgresql.org) and
  [SQLite](https://www.sqlite.org/) database bindings for the Primer
  language server's program store.

* `primer-service` adapts the `primer` Haskell API to an HTTP API
  (actually, [two different HTTP API's](http-apis.md)), so that it can
  be used over a network, and more specifically, in the case of the
  OpenAPI variant, easily consumed by popular web application
  frameworks.

(Additionally, the `primer-benchmark` package is used by the project
maintainers to track performance regressions and improvements, but
plays no part in the implementation of the Primer language service.)

The project also includes various tools for testing, linting, and
formatting code; and for managing the project's database schema. These
tools, and their related development workflows, are covered in detail
in other sections of this guide.

### Client applications

This project focuses on the Primer language service implementation.
Client applications that consume the service, whether by importing
Haskell modules from the `primer` package, or via one of the project's
two HTTP API's, are mostly out of scope for this project.

The `primer-service` package does include a trivial command-line
application, `primer-client`, which is written in Haskell and uses the
more powerful of the two HTTP API's. However, `primer-client` only
uses an extremely small subset of this API, and currently we only use
this client application to seed a new Primer server database with a
few simple programs. Over time, this client will likely evolve into
something more robust, but it's not intended to be a serious
demonstration or reference implementation of a Primer API client.

This project has an accompanying related project,
[`primer-app`](https://github.com/hackworthltd/primer-app), which
implements a full-featured web-based programming environment for the
Primer language. It uses the OpenAPI variant of this project's API, is
written in [TypeScript](https://www.typescriptlang.org), and uses
[React](https://react.dev) for its UI. Please see that project if
you're interested to learn how a full-featured Primer client
application is implemented.
