# Infix Operators

## Three Properties

Every operator has exactly three properties:

| Declaration | Meaning |
|---|---|
| `infixl 6 +` | Left-associative at precedence 6 |
| `infixr 0 $` | Right-associative at precedence 0 |
| `infix 4 ==` | Non-associative at precedence 4 |

Precedence: 0–9 (higher binds tighter)
Default: `infixl 9` (tightest, left-associative)

## Key Example: `$` and `.`

**Without `$` (precedence 0):**
```morloc
result = show (filter isPositive (map transform xs))
```

**With `$`:**
```morloc
infixr 0 $
result = show $ filter isPositive $ map transform xs
-- Parses as: show $ (filter isPositive $ (map transform xs))
```

**Composition `.` (precedence 9):**
```morloc
infixr 9 .
process = show . filter isPositive . map transform
result = process xs

-- Or combined:
result = show . filter isPositive . map transform $ xs
-- Parses as: (show . filter . map) $ xs
```

## Defining Custom Operators

```morloc
-- Fixity (position-independent)
infixl 6 |>

-- Type signature
(|>) a b :: a -> (a -> b) -> b

-- Implementation
source Py from "pipe.py" ("pipe" as (|>))

-- Usage
result = xs |> filter isPositive |> map transform
```

## Associativity Rules

| Left op | Right op | Result |
|---|---|---|
| `infixl 6 +` | `infixl 6 -` | OK — both left: `(a + b) - c` |
| `infixl 6 +` | `infixr 6 ++` | **Error** — conflicting assoc |
| `infix 4 ==` | `infix 4 !=` | **Error** — both non-assoc |

Non-associative chaining (`a == b == c`) is an error.

## Position Independence

Fixity declarations work regardless of order:

```morloc
-- All three orderings produce identical parse trees:

-- Usage first
x = "a" + 1 * 2
infixl 6 +
infixl 7 *

-- Fixity first
infixl 6 +
infixl 7 *
x = "a" + 1 * 2

-- Type first
(+) :: Str -> Int -> Str
infixl 6 +
x = "a" + 1 * 2
```

See [[FIXITY-BUGS.md]] for known implementation bugs.

## Quick Reference

| Situation | Behavior |
|---|---|
| No fixity declaration | `infixl 9` |
| Fixity after usage | Works (position-independent) |
| Duplicate fixity | **Error** |
| Mismatched assoc at same prec | **Error** |
| Non-assoc chaining | **Error** |
| Prefix position | `(+) 1 2` (function call) |
| Multiple operators | `infixl 6 +, -` |

---
*See also: [[MORLOC_SYNTAX.md]], [[PARSING-ALGORITHMS.md]], [[FIXITY-BUGS.md]]*
