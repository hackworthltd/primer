[![Build status](https://badge.buildkite.com/7a9fd1213265a375385deec0a418bc4a6f26b17b7f4efe3cad.svg?branch=main)](https://buildkite.com/hackworthltd/primer)

# Primer

Primer is a pedagogical functional programming language. This
repository contains Haskell implementations of:

* the core language;
* a database adapter for storing Primer programs; and
* a web service.

# Generating Axios bindings

We can automatically generate TypeScript Axios bindings for
`primer-service`:

```sh
make axios-bindings
```

This will place the generated bindings in the `axios-bindings`
subdirectory. (Note: do not check these bindings into this
repository.)
