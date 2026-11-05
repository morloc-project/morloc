# Function types: grouping, labels and docstrings

How a function type's arrows group. APP-1 rules that `a -> (b -> c)` and
`a -> b -> c` are one type; HOST makes grouping a property of a source.

- [read] A docstring on a parenthesized result group is shown as the
  return's (Docstrings.hs ~:207 pads or drops argument docs against the
  flat type). Needs a ruling on what such a docstring documents.
- [read] Host call grouping is carried by nesting: when a source's type
  variable is instantiated to a function (`feed_split :: ((Int -> b) -> c)
  -> c` at `b = Int -> Int`, golden spec-host-2-2), the nested function type
  inside the callback argument is what tells codegen to call the host's
  function one argument at a time. Any pass that flattens arrows inside
  arguments (a flattening FunU constructor, or `normalizeType` recursing
  into a FunU's parameters) silently breaks it, and `normalizeType`
  already flattens a FunU nested in a tuple, list or record (its AppU and
  NamU arms recurse) while it does not recurse into a FunU's own parameters
  or result, so a callback held in a tuple loses its grouping [read]. Spec gap: model/language
  host-calls.md says grouping is declared at the source, but the compiler
  reads it from the instantiated type's shape.
