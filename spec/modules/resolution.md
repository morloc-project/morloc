# Module Resolution

When a module imports another module by name, the compiler resolves that name to a file path. Resolution follows a three-case algorithm based on the relationship between the importing and imported module names.

## Case 1: No Context (Top-Level File)

When the entry-point file imports a module, or when no parent module context exists, the compiler searches three locations in order:

1. **Local**: `./foo.loc` or `./foo/main.loc` (relative to the importing file)
2. **System**: `$MORLOC_LIB/morloc/foo/main.loc` (core library)
3. **Plane**: `$MORLOC_LIB/<plane>/foo/main.loc` (plane-specific library)

The first match wins. `$MORLOC_LIB` defaults to `~/.local/share/morloc/src/`.

## Case 2: No Common Prefix

When module `foo.bar.baz` imports `bif.buf` and the names share no common prefix, the compiler searches only system and plane locations:

- `$MORLOC_LIB/morloc/bif/buf/main.loc`
- `$MORLOC_LIB/<plane>/bif/buf/main.loc`

Local paths are not searched in this case. This prevents a local file from accidentally shadowing a system library module.

## Case 3: Common Prefix

When module `foo.bar.baz` imports `foo.bif` and the names share a common prefix (`foo`), the compiler resolves the import relative to the shared prefix:

- From `foo/bar/baz/main.loc`, resolve `../../bif/main.loc`

This ensures that modules within the same package can reference siblings reliably, regardless of where the package is installed.

## Hierarchical Naming

Module names use dots as hierarchy separators: `foo.bar.baz`. Each segment maps to a directory level:

```
foo.bar.baz  -->  foo/bar/baz/main.loc
```

The leaf file is always `main.loc`.

## Plane Lookup

The default plane is `default`. The plane can be configured, allowing organizations to maintain private module namespaces. A module `foo` in plane `myorg` resolves to:

```
$MORLOC_LIB/myorg/foo/main.loc
```

## Search Order Summary

| Context | Search Order |
|---------|-------------|
| Top-level import | Local, System, Plane |
| No common prefix | System, Plane |
| Common prefix | Relative to prefix |
