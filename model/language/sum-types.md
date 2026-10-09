# Sum types

`data` declarations, constructors, recursive and parameterized data. Prefix: SUM.

### SUM-1 A `data` declaration lists a closed set of constructors
Intent: proposed
Code: unaudited

`data D a1 ... ak = C1 T11 ... | ... | Cn Tn1 ...` declares `D` with exactly
the constructors `C1 ... Cn`. Each `Ci` is a curried term of type
`Ti1 -> ... -> D a1 ... ak` and partially applies like any function.
Fields are positional and unnamed; matching is the only way to read them.

### SUM-2 A constructor determines its type
Intent: proposed
Code: unaudited

A constructor name belongs to one `data` type, so a constructor in an
expression fixes the type without a signature. Two `data` types in one scope
may not share a constructor name, and two constructors of one type may not
differ only in letter case. Constructors are exported and imported with
their type, and a qualified import qualifies them.

### SUM-3 A match over a `data` type is exhaustive and has no dead clause
Intent: proposed
Code: unaudited

A `|`-clause set or `match` that misses a constructor, or that has a clause
no value can reach, is a compile error. A catch-all `_` satisfies
exhaustiveness.

### SUM-4 A type cycle must pass through a `data` type
Intent: proposed
Code: unaudited

A `data` type may refer to itself, and several `data` types may refer to
each other. A cycle of type definitions made only of aliases and records is
a compile error.

### SUM-5 Comparison follows declaration order, in every language
Intent: proposed
Code: unaudited

`==` compares constructors, then fields. `<` orders constructors by their
position in the declaration, then the fields of equal constructors in order.
The result does not depend on the language the comparison runs in.

### SUM-6 A constructor pattern and the scrutinee's type
Intent: open
Code: unaudited

A constructor in an expression fixes its type (SUM-2), but a constructor in
a pattern does not: the scrutinee needs a type from a signature. Should a
constructor pattern fix the scrutinee's type the same way? Candidates:
(a) yes, symmetric with expressions; (b) no, and the compiler's rejection
asks for a signature.
