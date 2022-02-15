# Adding a new primitive type to Primer

See d4c4b9a for an example, where we add `Int`.

- Add a constructor to `PrimCon`.
  - Follow GHC's warnings to everywhere else that needs updating (textual name, tree rendering, type def).
- Add any desired primitive functions (technically _values_, but we're yet to have a use case for one without arguments) involving that type to `allPrimDefs`.
  - For each such function, add at least one test in `Tests.EvalFull`.
  - Adding a helper in `Primer.Core.DSL` (like `char`, `int`) may help with writing these functions.
- Add a case to the serialization outputs, with the new constructor.
- Some old tests may need IDs modified, due to the set of in-scope global variables having changed.