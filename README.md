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
