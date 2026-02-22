# Parsing

The parser transforms morloc source text into an indexed expression AST. It uses parser combinators with stateful tracking of fixity tables, expression indices, and indentation.

## Lexical Analysis

The lexer and parser are combined in a single pass using parser combinators (Megaparsec). There is no separate tokenization step; the parser directly consumes characters and builds AST nodes.

Key lexer features:

- **Indentation tracking**: the parser maintains a minimum indentation position. Continuation lines must be indented beyond the start of their expression.
- **Docstring recognition**: `--'` comments are captured as documentation and attached to subsequent declarations.
- **String interpolation**: `#{}` within string literals introduces embedded expressions.
- **Number formats**: decimal, hexadecimal (`0x`), octal (`0o`), and binary (`0b`) integer literals; decimal and scientific floating-point literals.

## Parser State

The parser carries mutable state through parsing:

| Field | Purpose |
|-------|---------|
| Fixity table | Maps operator names to (associativity, precedence) |
| Expression index | Counter for assigning unique IDs to AST nodes |
| Variable index | Counter for generating fresh variable names |
| Generics | Set of type variables in the current generic scope |
| Module config | Settings specific to the current module |
| Source positions | Maps expression IDs to source locations (for error reporting) |

## Top-Level Forms

The parser recognizes these top-level constructs:

1. **Module declaration**: `module name (exports)`
2. **Import**: `import name (terms)`
3. **Type signature**: `f a b :: Type`
4. **Function definition**: `f x y = body`
5. **Type alias**: `type Name = Type` or `type Lang => Name = "concrete"`
6. **Record/object/table declaration**: `record Name where ...`
7. **Typeclass declaration**: `class Name a where ...`
8. **Instance declaration**: `instance Name Type where ...`
9. **Fixity declaration**: `infixl 6 +, -`
10. **Source declaration**: `source Lang from "file" ("names")`

## Expression Parsing

Expressions are parsed by a layered grammar:

1. **Atoms**: literals, variables, parenthesized expressions, lists, tuples, records.
2. **Application**: juxtaposition of atoms (left-associative).
3. **Infix expressions**: operators interleaved with applications, resolved by precedence climbing.
4. **Lambda**: `\vars -> body`
5. **Where clauses**: `expr where bindings`

## Precedence Climbing

Infix operator parsing uses the precedence climbing algorithm (a form of Pratt parsing):

1. Parse a left-hand operand (application or atom).
2. Look ahead for an operator. If its precedence is below the current minimum, stop.
3. Consume the operator. Determine the minimum precedence for the right operand:
   - Left-associative: current precedence + 1
   - Right-associative: current precedence
   - Non-associative: current precedence + 1
4. Recursively parse the right operand at the new minimum.
5. Build an application node: `op(lhs, rhs)`.
6. Loop back to step 2 with the application as the new left-hand side.

All infix expressions desugar to prefix function application: `a + b` becomes `(+) a b`.

## Indexed Expressions

Every AST node is wrapped in an `ExprI` that pairs it with a unique integer index:

```
ExprI index Expr
```

These indices serve as keys for attaching metadata (source positions, type annotations, language tags) in later phases without modifying the AST structure.

## Module Parsing

Parsing is recursive over the module dependency graph:

1. Parse the entry-point file.
2. For each import, resolve the module path, parse it (accumulating state), and add it to the DAG.
3. Continue until all transitive imports are parsed.

The resulting DAG, combined with the accumulated parser state, is the input to the link phase.
