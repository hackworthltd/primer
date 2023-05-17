# The two APIs

There are two HTTP APIs in `primer-service`.

## The "Servant API"

First, there's what we informally call the "Servant API." This API is
implemented using Servant, obviously, and it exposes the full power &
capabilities of our Haskell implementation of the Primer programming
language. If you want to write a Primer API client in Haskell, you'll
almost certainly want to use this version of the API.

## The OpenAPI API

Unfortunately, Servant's remarkable ability to faithfully represent
the power of Haskell's type system via HTTP makes Servant APIs really
difficult to use with languages other than Haskell, at least without
quite a bit of mangling of the native Haskell types.

Therefore, `primer-service` provides a second, much simpler REST-ish
API, intended for use with clients written in pretty much any language
*other* than Haskell. Unlike the Servant API, this API comes with an
automatically generated OpenAPI 3 specification that makes it easy to
use with lots of web application frameworks.

The overarching design principle of the OpenAPI API is that we want
the client application to be mostly unburdened with concerns about
Primer's syntax and semantics. This allows implementers of Primer
OpenAPI clients to focus on a great UI and user experience (UX), and
leaves the really technical aspects of type checking, ASTs, editing
actions, etc., to the backend implementation.

The obvious use-case for this version of the API is our web-based
Primer frontend client, which lives in the
[`primer-app`](https://github.com/hackworthltd/primer-app) GitHub
repo and is written entirely in TypeScript.

# Generating an OpenAPI spec

To automatically generate the specification for `primer-service`'s
OpenAPI 3 API, run this command:

```sh
make openapi.json
```

This will place the generated spec in a file named `openapi.json`.

Note: this file should only be checked into git for use in our test
suite. That copy lives in the project's
`primer-service/test/outputs/OpenAPI` subdirectory.

It's also available via the Nix store:

```sh
nix build .#primer-openapi-spec
```
