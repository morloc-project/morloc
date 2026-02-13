# Lexical Structure

## Character Set

All morloc source files must contain only ASCII characters. Non-ASCII characters (including Unicode em-dashes, smart quotes, etc.) are not permitted and may cause silent truncation.

## Comments

```morloc
-- Line comment (to end of line)
--' Docstring comment (attaches to next declaration)
{- Block comment (nestable) -}
```

Docstring comments (`--'`) have semantic meaning: they provide documentation that propagates to generated CLI help text. See [[annotations.md]].

## Identifiers

**Term identifiers** begin with a lowercase letter or underscore, followed by alphanumeric characters, underscores, or single quotes:

```
foo, x', my_function, _unused
```

**Type identifiers** begin with an uppercase letter:

```
Int, Bool, List, Person
```

**Module names** consist of alphanumeric segments separated by hyphens:

```
root, root-py, root-cpp, math
```

**Operator identifiers** consist of one or more operator characters:

```
+  -  *  /  .  $  |>  ==  !=  >=  ++
```

Operator characters include: `! # % & * + - . / < = > ? @ \ ^ | ~ :`

## Literals

### Numeric Literals

```morloc
42          -- Int (decimal)
0xFF        -- Int (hexadecimal)
0o77        -- Int (octal)
0b1010      -- Int (binary)
3.14        -- Real (floating point)
1.0e-3      -- Real (scientific notation)
```

### String Literals

```morloc
"hello"              -- String literal
"line1\nline2"       -- Escape sequences: \n, \t, \\, \"
"value: #{expr}"     -- String interpolation
```

### Boolean Literals

```morloc
True
False
```

### Collection Literals

```morloc
[1, 2, 3]                      -- List
(1, "hello", True)              -- Tuple
{name = "Alice", age = 27}      -- Record
```

## Whitespace and Indentation

Morloc is indentation-sensitive, following conventions similar to Haskell:

- Top-level declarations must start at column 0.
- Continuation lines of an expression must be indented further than the start of that expression.
- `where` clauses introduce a new indentation block; all bindings in the block must align.

```morloc
foo x = result where
  helper y = y + 1    -- indented under where
  z = 42              -- aligned with helper
```

## Keywords

The following identifiers are reserved:

```
module  import  from  source  export  where  type  record  object  table
class  instance  infixl  infixr  infix  True  False
```

## Separators and Delimiters

```
(  )    -- Grouping, tuples, operator sections
[  ]    -- Lists
{  }    -- Records
,       -- Element separator
::      -- Type annotation
=       -- Definition, record field binding
->      -- Function type arrow, lambda arrow
\       -- Lambda introduction
.       -- Record field access (prefix), composition (infix)
$       -- Low-precedence application (infix)
=>      -- Language-specific type mapping
```
