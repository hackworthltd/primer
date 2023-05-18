# Frequently-asked questions (FAQ) about the project

*Note: this FAQ does not cover the syntax, semantics, or design of the
Primer programming language, nor does it cover this project's
accompanying client application project,
[`primer-app`](https://github.com/hackworthltd/primer-app). It is only
an FAQ for questions about this project, and this particular
implementation of the Primer programming language.*

## Why choose Haskell to implement the Primer backend service?

We chose Haskell for this project because we believe that Haskell is
the best programming language for writing programming languages, and
the bulk of the code in this project is effectively just that. The
database and HTTP API adapters are trivial compared to the type
checker, evaluator, and action implementations.

## Why not choose Haskell for the frontend implementation?

Our frontend application,
[`primer-app`](https://github.com/hackworthltd/primer-app), is
intended to run in the browser, and at the time of writing, Haskell is
still not a good enough browser application language for our needs. We
hope that [GHC
`9.6.1`](https://www.haskell.org/ghc/download_ghc_9_6_1.html) and
later versions will make it possible to run the code in the `primer`
package in the browser (i.e., the type checker, evaluator, and actions
engine), but it seems very unlikely that we would ever implement the
frontend UI in Haskell.

## Why this design? Why not just a standard compiler or interpreter?

One of our goals for this implementation of Primer is to ensure that
the requisite tools for programming &mdash; an editor, runtime,
debugger, etc. &mdash; can easily be incorporated into other
interactive applications, such as games and educational tools. This
goal is in support of a variety of contemporary approaches to teaching
programming and computer science in the context of other school
subjects, rather than only as a standalone discipline, in an effort to
increase the appeal and relevance of learning to program[^1][^2].

We believe the most egalitarian, accessible, and widely-available
platform is the Web, so we've designed our implementation around an
HTTP-based client-server model. Our backend can serve any client that
can speak HTTP, and our frontend is a collection of [React
components](https://react.dev) that can be used with popular web
application frameworks.

## How does this approach compare to so-called "scripting languages"?

Some programming languages do have good support for "embedding" into
other applications. These languages are sometimes called [*scripting
languages*](https://en.wikipedia.org/wiki/Scripting_language).
Probably the languages most frequently used for this kind of embedding
are [Lua](https://www.lua.org) and [Python](https://www.python.org)[^3].

Rarely is this feature used in the ways we have in mind for Primer
&mdash; most typically, these embedded languages are a mechanism for
extending the capabilities of an end-user application, akin to a
plug-in system, and not a means for writing domain-specific programs
as a primary activity &mdash; but in theory, it could be done.

However, for our purposes, existing scripting languages aren't a good
fit. Specifically:

* These languages aren't primarily designed for, or driven by,
  pedagogical aims: whatever their origins, they are now, first and
  foremost, general-purpose programming languages. To pick just one
  example of how this might adversely affect learning, the error
  messages that their interpreters produce may be difficult for
  novices to understand[^4].

* Whatever their merits in a pedagogical context, nearly all of these
  languages are imperative, not functional, which is counter to our
  goals with Primer. [Guile](https://www.gnu.org/software/guile/), an
  implementation of Scheme, is a notable exception. However, Guile is
  [dynamically
  typed](https://www.gnu.org/software/guile/manual/html_node/Dynamic-Types.html),
  which is also counter to our goals with Primer. We're not aware of
  another statically-typed functional programming language with robust
  support for embedding.

* These languages' implementations were not designed for "programming
  language as a service" applications. For example, their interpreters
  and other language facilities have no HTTP API; they're designed to
  be linked into the host application using an object code linker, and
  invoked via a C FFI[^5].

* These languages provide no built-in tools or support for editing or
  persisting programs. These facilities must be provided separately
  and out-of-band.

* These languages are completely text-based, with no support for
  visual programming.

---
[^1]: Guzdial, M. (2019). Computing for Other Disciplines. In S.
      Fincher & A. Robins (Eds.), The Cambridge Handbook of Computing
      Education Research (Cambridge Handbooks in Psychology, pp.
      584-605). Cambridge: Cambridge University Press.
      https://doi.org/10.1017/9781108654555.020

[^2]: Matthias Felleisen and Shriram Krishnamurthi. 2009. Viewpoint:
      Why computer science doesn't matter. Commun. ACM 52, 7 (July
      2009), 37–40. https://doi.org/10.1145/1538788.1538803
      [[PDF]](https://cs.brown.edu/~sk/Publications/Papers/Published/fk-why-cs-doesnt-matter/paper.pdf)

[^3]: This list is not meant to be comprehensive, with apologies to
      any languages we've elided.

[^4]: Brett A. Becker, Paul Denny, James Prather, Raymond Pettit,
      Robert Nix, and Catherine Mooney. 2021. Towards Assessing the
      Readability of Programming Error Messages. In Proceedings of the
      23rd Australasian Computing Education Conference (ACE '21).
      Association for Computing Machinery, New York, NY, USA, 181–188.
      https://doi.org/10.1145/3441636.3442320
      [[PDF]](https://dl.acm.org/doi/pdf/10.1145/3441636.3442320)

[^5]: (This "legacy"-style embedding may become less of an impediment
      to in-browser use as
      [WebAssembly](https://developer.mozilla.org/en-US/docs/WebAssembly)
      and its various bridge technologies like
      [Emscripten](https://developer.mozilla.org/en-US/docs/WebAssembly/C_to_wasm)
      mature, but of course, our approach will be amenable to these
      techniques, as well.)
