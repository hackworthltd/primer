# Haskell style guide for the Primer project

This document provides rules and guidelines for contributing Haskell and Haskell-related content to the Primer project.

Over time, we may distinguish between requirements/rules and strong preferences. For now, consider this guide to be advisory &mdash; there are currently areas where even code written by members of Hackworth Ltd doesn't live up to these standards!

Note that this style guide doesn't apply to code written *in the Primer language* itself; e.g., code written for the Primer prelude is out of scope.

## Enforcement

When possible, we use continuous integration (CI) checks to enforce our style guide, so in many cases, deviations from this guide will be flagged automatically. Otherwise, if you make a contribution that deviates from the guide, a project maintainer may ask you to make changes to your code. In some cases, the maintainer may simply make the necessary changes themselves, and then co-commit the joint contribution to the project's `main` branch.

## Consider the audience

Primer is chiefly intended for use by children and young adults from all over the world. Please keep this in mind when writing code, comments, and documentation, and especially so when writing content that students will see and/or interact with.

## Semantic versioning

Once this project reaches 1.0 status, we will enforce some form of [semantic versioning](https://en.wikipedia.org/wiki/Software_versioning#Semantic_versioning). Our current plan of record is to support the [Haskell PVP](https://pvp.haskell.org).

However, until we reach that status, we can only commit to making a best-effort attempt to adhere to PVP/SemVer. Expect [Hackage releases of this project](https://hackage.haskell.org/package/primer) to lag significantly behind the `main` branch until we near the 1.0 release of the project.

## Haskell code

### Imports and exports

* Implicit importing of `Prelude` is disabled project-wide. Do not import it; use [`Foreword`](../primer/src/Foreword.hs), our custom prelude, instead.
* Always use explicit exports in libraries and executables.
* Imports should either be qualified (e.g., `import Data.Map.Strict qualified as M`), or given as an explicit import list (e.g., `import Foo (a, b)`).
* As an exception to the above rule, the contents of [`Foreword`](../primer/src/Foreword.hs) can be imported unqualified (i.e., `import Foreword`).
* Except for import-for-reexport tricks, every qualified import should be imported under a unique name.
* Explicit imports in tests are encouraged, but optional, as we want to keep the friction of writing & maintaining tests to a minimum.

### Typeclasses & deriving

* When it makes sense to do so, provide useful `deriving` instances for types. We suggest at least `Show`, `Generic`, and, when appropriate, `Eq`.
* Unless there's a good reason not to do so, preserve the identity between a type's `Show` and `Read` instances.
* Types that need to be serialized to & from the database and/or API will additionally need [`FromJSON`](https://hackage.haskell.org/package/aeson-2.1.2.1/docs/Data-Aeson.html#t:FromJSON) and [`ToJSON`](https://hackage.haskell.org/package/aeson-2.1.2.1/docs/Data-Aeson.html#t:ToJSON) instances. You should use `deriving (FromJSON, ToJSON) via PrimerJSON ...` to implement these instances; see the [Primer.JSON](../primer/src/Primer/JSON.hs) module for details.
* Do not use orphan instances in libraries and executables unless absolutely necessary. (Note that there are a few cases where we do purposely use orphan instances in this project.)
* When orphan instances are necessary, prefer placing them in a separate module whose leaf name is `Orphans.hs`.

### Naming

* We encourage the use of the [`DuplicateRecordFields`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/duplicate_record_fields.html), [`OverloadedRecordDot`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html), and similar GHC extensions to avoid annoying name clashes in record types.
* Don't use leading underscores (`_`) in names, unless it's necessary to avoid a name clash. (This guideline applies not only to lenses, but also more generally.)

### Lenses

* We use the [`optics`](https://hackage.haskell.org/package/optics) family of lenses, rather than [`lens`](https://hackage.haskell.org/package/lens), mainly for more ergonomic error messages.
* We encourage the use of lenses in code, and recommend writing new lenses for our own types, when it makes sense to do so.

### Banned code

* This implementation of Primer is meant to be used as a highly-available online service, so it should never crash unless absolutely necessary. (An example of a rare case where crashing may be acceptable is when a database integrity issue is detected, and continuing to operate might otherwise exacerbate the issue.) Therefore, don't use [`error`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#v:error), [`undefined`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#v:undefined), or similar crashing actions, in libraries or executables.
* For similar reasons, don't use partial functions in libraries or executables.
* Crashing and/or partial functions may be used in tests, when it makes sense to do so.

### Logging

* This implementation of Primer is meant to be used as a service, and more specifically, to run in a container. Therefore, we make extensive use of logging.
* That said, logs should be output with the [standard `syslog` severity levels](https://en.wikipedia.org/wiki/Syslog#Severity_level) in mind (debugging, informational, emergency, etc.), and should not be more verbose than necessary.
* When outputting any logs, use the standard logging framework that we've designed, which will take care of eliding levels, formatting, deciding where to output the logs, etc. See the [Primer.Log](../primer/src/Primer/Log.hs) module for details.
* Never write directly to `stderr` or `stdout`, unless something must be shown to the operator prior to logging being available; e.g., during command line parsing.

### Comments

* All exported types, terms, etc., should include at least a single-line comment that describes their purpose.
* When writing comments, make use of proper [Haddock markup](https://haskell-haddock.readthedocs.io/en/latest/markup.html).
* Don't leave notices like `BUG:` or `TODO:` in comments without a link to a corresponding [GitHub Issue](https://github.com/hackworthltd/primer/issues), and file a new one, if needed. We use this mechanism to ensure that, once a bug or unimplemented feature is addressed in the tracker, we can locate all the occurrences of it in the code base.
* When linking to a [GitHub Issue](https://github.com/hackworthltd/primer/issues) in this repo in a comment, don't use the short-form bare hash (e.g., `#975`) that GitHub automatically expands when viewed in its web UI. Instead, use a proper URI to link to the issue (e.g., [`https://github.com/hackworthltd/primer/issues/975`](https://github.com/hackworthltd/primer/issues/975)). For one thing, these proper URI's are useful in text editors and other code browsing interfaces other than GitHub's own. Additionally, if we were to import the source code into a new repo (or even a new hosting service), those short hashes would no longer make any sense.

### Style

* When a module uses [`Foreword`](../primer/src/Foreword.hs), it should always lead with that import, followed by a blank line, like so:
```haskell
import Foreword

...
```
* Avoid Template Haskell if possible, as it makes cross-compilation more difficult.
* Limit line length to 100 characters.
* Avoid using semicolons (`;`) to string actions together on a single line.
* We use [Fourmolu](https://hackage.haskell.org/package/fourmolu) to format our code. See the project's [`fourmolu.yaml`](../fourmolu.yaml) for our project-wide settings.
* We use [HLint](https://hackage.haskell.org/package/hlint) for linting. Our HLint settings are fairly opinionated; see [`.hlint.yaml`](../.hlint.yaml).
* Code must pass project-specific linting & formatting checks before being committed to the `main` branch. (Occasional exceptions may be made.)

## Cabal

* Start all new Cabal projects with `default-language: GHC2021`.
* Frequently-enabled GHC extensions should be considered for use in Cabal's `default-extensions` stanza.

## Tests

* Code must pass all tests before being committed to the `main` branch. We may occasionally disable a test to work around a known issue with tooling or other externalities.
* When adding new functionality, write at least a few unit/property tests for it, unless it's completely trivial. We especially appreciate tests that make use of the facilities in [`Hedgehog.Gen`](https://hackage.haskell.org/package/hedgehog-1.2/docs/Hedgehog-Gen.html).
* We encourage the use of the `HasCallStack` constraint in tests, as it may make failures easier to debug.

## GHC compatibility

* We can't make promises on backward compatibility with GHC releases older than the one we're currently using to develop the project, but we won't break compatibility arbitrarily.
* We will make a best-effort attempt to test all supported GHC versions in CI.
* As a general rule, we would like to avoid CPP-style conditional compilation, except as a last resort.
* We expect the 1.0 release of the project to have a minimum requirement of at least [GHC `9.6.1`](https://www.haskell.org/ghc/download_ghc_9_6_1.html), as we want to take advantage of that version's support for [JavaScript](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html) and [WebAssembly](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html?highlight=wasm) targets.
* We do not plan to support Haskell implementations other than GHC.

## Spelling & grammar

* Within reason, please use proper grammar and punctuation in comments, documentation, etc.
* Please use [American English spelling](https://en.wikipedia.org/wiki/American_and_British_English_spelling_differences) for consistency with the majority of open source projects on the Internet. (See [this comment](https://github.com/hackworthltd/primer/pull/195#discussion_r747472067) for reference.)

## Internationalization (i18n)

The Primer programming language has been designed with the intention of supporting natural languages other than English. However, for the most part, we assume that students who're using Primer will not be interacting directly with the code in this project, and instead will be using a graphical frontend interface, such as the one implemented by the [`primer-app`](https://github.com/hackworthltd/primer-app) project.

We believe that most internationalization (i18n) issues can be deferred to such frontend projects, and don't need to be dealt with in this project. Indeed, the Primer API has been designed with this separation of concerns in mind. Therefore, at this time, we don't have any particular internationalization requirements or guidelines for this project, though this may change in the future as required.

## Accessibility (a11y)

The Primer programming language has been designed with accessibility (a11y) in mind. For example, we believe that our tree-oriented structure editor approach is a good fit for screen reader applications. However, as with internationalization requirements, we believe that most accessibility concerns can be handled in Primer frontend projects, and need not be directly addressed in this project, except where we have made decisions in the API design to accommodate accessibility requirements. In any case, at this time, we don't have any particular accessibility requirements or guidelines for this project, except that all design decisions should continue to be made with accessibility needs in mind.
