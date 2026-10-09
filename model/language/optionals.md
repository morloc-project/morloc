# Optionals

`?T`: missing values, idempotence, native null. Prefix: OPT.

### OPT-1 `?T` is a `T` or `Null`
Intent: proposed
Code: unaudited

`?` applies to any type. `Null` is the only absent value and has type `?T`
for every `T`.

### OPT-2 `?T` is idempotent
Intent: proposed
Code: unaudited

    ?(?T) == ?T

There is one `Null`, and no value distinguishes an outer from an inner
missing level. Layered missingness is expressed with a sum type or a
library type.

Why: `?T` lowers to each language's structureless native null.

### OPT-3 A `T` is accepted where a `?T` is expected
Intent: proposed
Code: unaudited

    T <: ?T

The coercion needs no wrapper at the use site and holds across a language
boundary. Whether it lifts through other type constructors is SUB-5.

### OPT-4 `?T` is the language's own missing value
Intent: proposed
Code: unaudited

`[[Null]]_Py` is `None`, `[[Null]]_R` is `NULL`, and `[[?T]]_Cpp` is
`std::optional<[[T]]_Cpp>`. A present value is the plain `[[T]]_L` in
Python and R.

### OPT-5 A missing element inside an R vector
Intent: open
Code: unaudited

R's `NULL` cannot be an element of an atomic vector. What is
`[[ [?Int] ]]_R`? Candidates: (a) a list with `NULL` elements; (b) an atomic
vector with `NA` for `Null`, which makes `NA` and `Null` one value in R.

### OPT-6 Argument order and a `T` meeting a `?T` at one type variable
Intent: open
Code: unaudited

With `same :: a -> a -> Bool`, `x : H` and `y : ?H`:

    same y x    accepted today: a == ?H, x coerced (OPT-3)
    same x y    rejected today: a == H, and not (?H <: H)

List literals behave the same way (`[y, x]` is accepted, `[x, y]` is
rejected). The branches of an `if` are accepted in either order. Candidates:
(a) order-independent: a variable met by both `T` and `?T` is `?T`, and the
`T` argument is coerced, as in Kotlin, Swift, C#, Dart and TypeScript;
(b) left to right, as today, with an error that names the earlier argument
and suggests `(x :: ?H)`. (a) leaves the typing of every program accepted
today unchanged. Haskell and OCaml have no such coercion and reject both
orders.
