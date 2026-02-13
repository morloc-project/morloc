# CLI Generation

Morloc automatically generates command-line interfaces from exported function signatures and their annotations. Each exported function becomes a subcommand of the compiled program.

## Basic Structure

A compiled program `foo` with exported functions `bar` and `baz` produces:

```
./foo bar [args]
./foo baz [args]
./foo --help
./foo bar --help
```

## Argument Mapping

Function parameters map to CLI arguments based on their types and annotations.

### Positional Arguments

By default, function parameters become positional arguments in declaration order:

```morloc
--' Scale a value
scale :: Real -> Real -> Real
```

```
./foo scale 3.14 2.0
```

### Optional Arguments

Parameters annotated with `@opt` become named optional arguments with defaults:

```morloc
"""
@opt n Number of iterations (default: 10)
"""
iterate :: Int -> [Real] -> [Real]
```

```
./foo iterate --n=20 "[1.0, 2.0]"
```

### Flags

`Bool` parameters annotated with `@flag` become boolean flags:

```morloc
"""
@flag verbose Enable verbose output
"""
process :: Bool -> [Str] -> [Str]
```

```
./foo process --verbose '["a", "b"]'
./foo process --no-verbose '["a", "b"]'
```

### Record Arguments

When a function takes a record type, its fields are expanded into a group of CLI options:

```morloc
record Config where
  threshold :: Real
  maxIter :: Int

--' Run analysis
analyze :: Config -> [Real] -> [Real]
```

```
./foo analyze --threshold=0.5 --max-iter=100 "[1.0, 2.0]"
```

## Help Text

Help text is generated from:

- The function's docstring (description)
- `@arg` annotations (per-argument descriptions)
- Type signatures (type information and metavar names)
- Default values (for optional arguments and flags)

```
$ ./foo bar --help
bar - Scale a value

Usage: foo bar <x> <y>

Arguments:
  x    First number (Real)
  y    Second number (Real)
```

## Input Format

Arguments are parsed according to their type schemas:

- **Numeric types**: parsed as numbers (`42`, `3.14`)
- **Strings**: parsed as-is or quoted
- **Booleans**: `true`/`false` for positional; `--flag`/`--no-flag` for flags
- **Lists**: JSON array syntax (`[1, 2, 3]`)
- **Records**: JSON object syntax or expanded into named options
- **Tuples**: JSON array syntax (`[1, "hello"]`)

## Output Format

Return values are printed to stdout. The nexus deserializes the pool's msgpack response and renders it as human-readable text. Strings are printed without quotes; numbers, lists, and records are printed in JSON-like format.
