# Operators

Morloc supports user-defined infix operators with explicit precedence and associativity declarations.

## Operator Characters

Operators are identifiers composed entirely of the characters:

```
! # % & * + - . / < = > ? @ \ ^ | ~ :
```

Examples: `+`, `*`, `.`, `$`, `|>`, `==`, `!=`, `>=`, `++`, `>>=`

## Fixity Declarations

Every operator has three properties: associativity, precedence, and name. These are declared with fixity statements:

```morloc
infixl 6 +, -       -- left-associative, precedence 6
infixr 5 :          -- right-associative, precedence 5
infix  4 ==, !=     -- non-associative, precedence 4
```

**Precedence** ranges from 0 (loosest) to 9 (tightest). The default for an undeclared operator is `infixl 9`.

**Associativity** determines grouping when operators of the same precedence appear in sequence:

| Associativity | Grouping | Example |
|---------------|----------|---------|
| `infixl` (left) | `(a + b) + c` | arithmetic operators |
| `infixr` (right) | `a : (b : c)` | cons, composition |
| `infix` (none) | `a == b == c` is an **error** | comparison operators |

## Precedence Rules

When two operators appear in the same expression, the one with higher precedence binds tighter:

```morloc
infixl 7 *
infixl 6 +

1 + 2 * 3       -- parses as: 1 + (2 * 3)
```

When two operators have the same precedence:

- If both are `infixl`, they group left: `(a + b) - c`
- If both are `infixr`, they group right: `a : (b : c)`
- If they have conflicting associativity or are both `infix`, it is a **parse error**

## Standard Operators

| Operator | Fixity | Purpose |
|----------|--------|---------|
| `.` | `infixr 9` | Function composition |
| `$` | `infixr 0` | Low-precedence application |
| `+`, `-` | `infixl 6` | Arithmetic |
| `*`, `/` | `infixl 7` | Arithmetic |
| `==`, `!=` | `infix 4` | Comparison |

## Desugaring

All infix expressions desugar to prefix function application:

```morloc
a + b          -- desugars to: (+) a b
a + b * c      -- desugars to: (+) a ((*) b c)
```

Operators in prefix position are enclosed in parentheses:

```morloc
(+) 1 2        -- prefix application
```

## Defining Custom Operators

A custom operator requires a fixity declaration, a type signature, and an implementation:

```morloc
infixl 6 |>

(|>) a b :: a -> (a -> b) -> b
source Py from "pipe.py" ("pipe" as (|>))

result = xs |> filter isPositive |> map transform
```

## Position Independence

Fixity declarations may appear anywhere in the module -- before or after the expressions that use the operator. The parser applies fixity information retroactively.

## Precedence Climbing

The parser uses a precedence climbing algorithm (Pratt parsing) to resolve operator expressions. The algorithm recursively parses operands, accepting only operators whose precedence meets or exceeds a minimum threshold. Left-associative operators increment the threshold for the right operand; right-associative operators do not.
