# Annotations

Morloc uses docstring comments and metadata tags to annotate exported functions. These annotations drive automatic CLI generation: the nexus translates them into command-line argument parsers, help text, and usage messages.

## Docstring Syntax

A docstring comment is a single line beginning with `--'` immediately preceding a type signature:

```morloc
--' Add two numbers
add :: Int -> Int -> Int
```

Multi-line docstrings use consecutive `--'` lines:

```morloc
--' Compute the mean of a list of numbers
--' Returns 0 for an empty list
mean :: [Real] -> Real
```

Triple-quoted block docstrings use `"""`:

```morloc
"""
Add two numbers

@arg x First number
@arg y Second number
"""
add :: Int -> Int -> Int
```

## Metadata Tags

Metadata tags within docstrings control how function arguments map to CLI parameters.

### `@arg`

Associates a name and description with a positional argument:

```morloc
"""
@arg x The input value
@arg y The scaling factor
"""
scale :: Real -> Real -> Real
```

This generates: `./program scale <x> <y>`

### `@opt`

Marks an argument as an optional CLI parameter with a default value:

```morloc
"""
@opt n Number of iterations (default: 10)
"""
iterate :: Int -> [Real] -> [Real]
```

This generates: `./program iterate --n=10 <input>`

### `@flag`

Marks a `Bool` argument as a flag:

```morloc
"""
@flag verbose Enable verbose output
"""
process :: Bool -> [Str] -> [Str]
```

This generates: `./program process --verbose <input>`

Flags also generate a negation form: `--no-verbose`.

## Effect on CLI Generation

When a function is exported from the main module, its docstring and type signature together determine the generated CLI interface:

- Each function becomes a subcommand: `./program <function> [args]`
- Positional arguments follow the subcommand in order
- Optional arguments use `--name=value` syntax
- Flags use `--name` / `--no-name` syntax
- Record arguments are expanded into grouped options
- Help text is derived from docstring content

See [[../runtime/cli.md]] for the full CLI generation rules.
