# Function types: grouping, labels and docstrings

How a function type's arrows group. APP-1 rules that `a -> (b -> c)` and
`a -> b -> c` are one type; HOST makes grouping a property of a source.

- [reproduced] A label in a parenthesized result group is refused:
  `f :: Int -> (n@Int -> V n)` fails "the label 'n@' binds nothing here"
  (Parser.y `nestedLabel`), while `Int -> n@Int -> V n` is accepted. Open:
  APP-1 makes `n@Int` a whole argument of the signature, POLY-3 lets only
  a whole signature argument carry a label, but the grammar sees a nested
  type. Needs a ruling. Reproduction: repro/poly7/l2.loc.
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
- [read] Open, DIM-10: a Nat in an argument of a function the result holds
  (`run :: Int -> (V n -> Int, Int)`, `Int -> [V n -> Int]`) counts as
  one the function chooses, since only the result has it; `run k = (g, k)`
  with `g :: V 3 -> Int` is accepted and typed `Int -> (V 3 -> Int, Int)`.
  Sound as an existential (a caller cannot apply the function to a length
  it picks), but the signature reads as if the caller chose n. Ruling
  needed: polarity (a Nat at any inward position is the caller's) or
  DIM-10 as written. A polarity rule also changes callback arguments
  (`apply :: (V n -> Int) -> Int` would let `apply` choose n) and mixed
  packages (`Int -> (V n, V n -> Int)` would become rigid).
