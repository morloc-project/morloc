# Manifest Schema

The manifest is a JSON file that drives the nexus at runtime. It describes the pools, commands, argument schemas, and pure expression trees for one compiled morloc program.

## Top-Level Structure

```json
{
  "version": 1,
  "pools": [ ... ],
  "commands": [ ... ]
}
```

## Pool Definitions

Each pool entry describes a language-specific worker process:

```json
{
  "lang": "cpp",                   -- language identifier
  "exec": ["./pool-cpp.out"],      -- command to start the pool
  "socket": "pipe-cpp"             -- Unix socket basename
}
```

| Field | Type | Description |
|-------|------|-------------|
| `lang` | string | Language name (`"cpp"`, `"python3"`, `"r"`) |
| `exec` | [string] | Command and arguments to start the pool |
| `socket` | string | Socket filename (created in temp directory) |

## Command Definitions

Each command corresponds to an exported function. Commands come in two forms.

### Remote Commands

A remote command dispatches to a pool:

```json
{
  "name": "foo",
  "type": "remote",
  "mid": 1,                        -- manifold ID for dispatch
  "pool": 0,                       -- index into pools array
  "needed_pools": [0],             -- pool indices to start
  "arg_schemas": ["<double>f8"],   -- msgpack schemas per argument
  "return_schema": "<double>f8",   -- msgpack schema for return value
  "desc": ["description"],         -- docstring lines
  "return_type": "Real",           -- display type name
  "return_desc": [],               -- return value documentation
  "args": [ ... ]                  -- argument definitions
}
```

### Pure Commands

A pure command is evaluated by the nexus without any pool:

```json
{
  "name": "greeting",
  "type": "pure",
  "arg_schemas": ["s"],
  "return_schema": "s",
  "desc": [],
  "return_type": "Str",
  "return_desc": [],
  "args": [ ... ],
  "expr": { ... }                  -- expression tree
}
```

## Argument Definitions

### Positional

```json
{
  "kind": "pos",
  "metavar": "X",
  "type_desc": "Real",
  "quoted": false,
  "desc": []
}
```

### Optional

```json
{
  "kind": "opt",
  "metavar": "N",
  "type_desc": "Int",
  "quoted": false,
  "short": "n",
  "long": "count",
  "default": "1",
  "desc": []
}
```

### Flag

```json
{
  "kind": "flag",
  "short": "v",
  "long": "verbose",
  "long_rev": "no-verbose",
  "default": "false",
  "desc": []
}
```

### Group (Record Argument)

```json
{
  "kind": "grp",
  "metavar": "CONFIG",
  "desc": [],
  "group_opt": {"short": null, "long": "config"},
  "entries": [
    {"key": "x", "arg": { ... }}
  ]
}
```

## Expression Tree

Pure commands contain an expression tree that the nexus evaluates directly. Node types:

| Tag | Fields | Description |
|-----|--------|-------------|
| `lit` | `schema`, `lit_type`, `value` | Numeric/boolean literal |
| `str` | `schema`, `value` | String literal |
| `container` | `schema`, `elements` | List or tuple |
| `app` | `schema`, `func`, `args` | Function application |
| `lambda` | `vars`, `body` | Lambda abstraction |
| `bound` | `schema`, `var` | Bound variable reference |
| `interpolation` | `schema`, `strings` | String interpolation |
| `pattern` | `schema`, `pattern` | Accessor/setter pattern |

### Selector Patterns

Patterns use selectors for structural access:

```json
{"type": "end"}                                            -- terminal
{"type": "idx", "selectors": [{"index": 0, "sub": ...}]}  -- tuple index
{"type": "key", "selectors": [{"key": "name", "sub": ...}]} -- record key
```
