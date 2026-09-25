# Expressions

Morloc expressions describe data transformations. Every expression has a type, inferred or checked by the type system.

## Function Application

Application is by juxtaposition. Arguments follow the function, separated by whitespace:

```morloc
f x
map add [1, 2, 3]
add 1 2
```

Application is left-associative: `f x y` parses as `(f x) y`.

Functions may be partially applied:

```morloc
addFive = add 5       -- partial application of add
increment = (+) 1     -- partial application of operator
```

## Lambda Expressions

Lambdas are introduced with `\` and use `->` to separate parameters from the body:

```morloc
\x -> x + 1
\x y -> x + y
```

Lambdas may appear anywhere an expression is expected:

```morloc
map (\x -> x * 2) xs
```

## Function Composition

The `.` operator composes functions right-to-left:

```morloc
process = show . filter isPositive . map transform
```

The above is equivalent to `\x -> show (filter isPositive (map transform x))`.

The `$` operator provides low-precedence right-associative application, reducing parentheses:

```morloc
result = show $ filter isPositive $ map transform xs
```

## Where Clauses

A `where` clause introduces local bindings scoped to the enclosing definition:

```morloc
hypotenuse a b = sqrt (sqA + sqB) where
  sqA = a * a
  sqB = b * b
```

Local bindings may be functions:

```morloc
foo x = result where
  helper y = y + 1
  result = helper (helper x)
```

A `where` binding is lexically scoped: it shadows a top-level term of the
same name, and a variable at a use site never captures it.

## Let Expressions

`let` binds names in an expression, one after another:

```morloc
twice x = let a = costly x in a + a

pipeline x =
  let a = step1 x
      b = step2 a
  in a + b
```

Each binding is in scope in the bindings after it and in the body.

## When Named Values Are Evaluated

Morloc is strict: every argument is evaluated once, where it is applied,
however many times its parameter is used (zero included). A named value
follows the same rule. It is computed at most once per evaluation of the
scope that binds it, never once per mention:

- **Placement:** a `where` or `let` binding is computed at the nearest
  point every use passes through. A binding read in only one arm of a
  conditional is computed in that arm; one read in a condition is computed
  before it; one read only inside `@try` is computed inside it, so its
  failure is caught there.
- **Unused bindings:** a `where` binding that nothing reads is never
  computed. A `let` binding that nothing reads is still computed where it
  is written, since the function it calls may act in ways no type records.
- **Lambdas and do-blocks:** a pure value used inside a lambda or a do-block
  is computed when the lambda or do-block is built, and captured. Forcing a
  suspension twice reruns its effects, not the pure values it captured.
- **Top-level constants:** a top-level value that is not a function (or is
  a function built by a computation, such as a partial application) is
  computed at most once per command.
- **Languages:** a named value is one value in every language that reads it.
  It is computed once, in one language, and passed to its other readers.

A function definition is code, not a value: it may be copied or shared
between its uses freely, since calling it computes the same thing either way
(see [[../interop/implementation-selection.md]]).

## Record Field Access

The `.` operator in prefix position accesses a record field:

```morloc
.name alice        -- extract the "name" field
.age alice         -- extract the "age" field
```

Field accessors may be composed:

```morloc
getName = .name
names = map getName people
```

## Record Construction

Records are constructed with brace syntax:

```morloc
alice = {name = "Alice", age = 27}
```

## Tuple Construction

Tuples are constructed with parentheses:

```morloc
pair = (1, "hello")
triple = (True, 3.14, "x")
```

## List Construction

Lists use bracket syntax:

```morloc
xs = [1, 2, 3]
empty = []
nested = [[1, 2], [3, 4]]
```

## Type Ascription

An expression may be annotated with its type using `::`:

```morloc
(42 :: Int)
```

## Operator Sections

Operators can be used in prefix position by enclosing them in parentheses:

```morloc
(+) 1 2       -- prefix application
(+ 1)         -- right section: \x -> x + 1
```

## Conditionals

A guard chooses among expressions, checked in order, with `:` for the
remaining case:

```morloc
clamp lo hi x
  ? x < lo = lo
  ? x > hi = hi
  : x
```

Only the chosen expression is evaluated.

## Limitations

- **No native side effects.** All side effects originate in foreign implementations and are surfaced through effect annotations on source signatures. See [[../types/effects.md]].
