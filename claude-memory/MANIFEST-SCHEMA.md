# Manifest JSON Schema

```json
{
  "version": 1,

  "pools": [
    {
      "lang": "cpp",                    // language name
      "exec": ["./pool-cpp.out"],       // command to start pool
      "socket": "pipe-cpp"              // socket basename
    }
  ],

  "commands": [
    {
      // --- Remote command (calls a pool) ---
      "name": "foo",
      "type": "remote",
      "mid": 1,                         // manifold ID for dispatch
      "pool": 0,                        // index into pools[]
      "needed_pools": [0],              // pool indices to start
      "arg_schemas": ["<double>f8"],    // msgpack schemas per arg
      "return_schema": "<double>f8",    // msgpack schema for return
      "desc": ["description line"],     // docstring lines
      "return_type": "Real",            // display type name
      "return_desc": [],                // return value docstring
      "args": [ /* see Argument Kinds below */ ]
    },
    {
      // --- Pure command (no pool, expression tree) ---
      "name": "greeting",
      "type": "pure",
      "arg_schemas": ["s"],
      "return_schema": "s",
      "desc": [],
      "return_type": "Str",
      "return_desc": [],
      "args": [ /* see Argument Kinds below */ ],
      "expr": { /* see Expression Tree below */ }
    }
  ]
}
```

## Argument Kinds

```json
// Positional
{ "kind": "pos", "metavar": "X" | null, "type_desc": "Real",
  "quoted": false, "desc": [] }

// Optional
{ "kind": "opt", "metavar": "N", "type_desc": "Int", "quoted": false,
  "short": "n" | null, "long": "count" | null, "default": "1", "desc": [] }

// Flag
{ "kind": "flag", "short": "v" | null, "long": "verbose" | null,
  "long_rev": "no-verbose" | null, "default": "false", "desc": [] }

// Group (record arg)
{ "kind": "grp", "metavar": "CONFIG", "desc": [],
  "group_opt": { "short": null, "long": "config" } | null,
  "entries": [ { "key": "x", "arg": { /* nested opt or flag */ } } ] }
```

## Expression Tree

Maps 1:1 to `make_morloc_*()` functions in `eval.c`:

```json
// Literal
{"tag": "lit", "schema": "i8", "lit_type": "i8", "value": "42"}

// String literal
{"tag": "str", "schema": "s", "value": "hello"}

// Container (list/tuple)
{"tag": "container", "schema": "[i8]", "elements": [/* nodes */]}

// Function application
{"tag": "app", "schema": "s", "func": {/* node */}, "args": [/* nodes */]}

// Lambda
{"tag": "lambda", "vars": ["x", "y"], "body": {/* node */}}

// Bound variable
{"tag": "bound", "schema": "s", "var": "x"}

// String interpolation
{"tag": "interpolation", "schema": "s", "strings": ["hi ", "!"]}

// Pattern (accessor/setter)
{"tag": "pattern", "schema": "...", "pattern": {/* selector */}}
```

### Selectors

```json
{"type": "end"}
{"type": "idx", "selectors": [{"index": 0, "sub": {/* selector */}}]}
{"type": "key", "selectors": [{"key": "name", "sub": {/* selector */}}]}
```

## Key Files

- **Writer**: `library/Morloc/CodeGenerator/Nexus.hs` (~578 lines)
- **Reader**: `data/morloc/manifest.c` + `data/morloc/manifest.h`
- **C structs**: `manifest_t`, `manifest_command_t`, `manifest_arg_t`, `manifest_pool_t`
