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

There is no `let ... in` syntax; use `where` instead.

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

## Limitations

- **No recursion.** Use higher-order functions (`map`, `fold`, `filter`) for iteration.
- **No conditionals.** Use pattern matching or foreign functions for branching.
- **No side effects.** Morloc expressions are pure; effects are confined to foreign implementations.
