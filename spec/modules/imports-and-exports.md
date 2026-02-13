# Imports and Exports

## Module Declaration

A module declaration names the module and lists its exports:

```morloc
module main (foo, bar)
```

The export list determines which names are visible to importers. The wildcard `(*)` exports all top-level definitions:

```morloc
module utilities (*)
```

If no module declaration is present, the file is an anonymous module. Anonymous modules cannot be imported by other modules but may serve as the entry point for `morloc make`.

## Export Lists

Only names listed in the export list are available to importing modules. This includes:
- Function names
- Type names
- Operator names (in parentheses)

```morloc
module math (sin, cos, pi, (+))
```

Names not in the export list are private to the module.

## Import Forms

### Import All

Import all exports from a module:

```morloc
import root-py
```

### Selective Import

Import specific names:

```morloc
import foo (bar, baz)
```

Only `bar` and `baz` are brought into scope. Other exports of `foo` are not accessible.

### Multiple Imports of the Same Name

Importing the same function name from multiple language-specific modules creates implementation polymorphism:

```morloc
import root-py (map, filter, fold)
import root-cpp (map, filter, fold)
import root-r (map, filter, fold)
```

After these imports, `map`, `filter`, and `fold` each have three language implementations. The compiler selects among them during realization. See [[../interop/implementation-selection.md]].

## Visibility Rules

- A name is visible in a module if it is defined there or imported.
- Imported names do not automatically re-export. To re-export, the name must appear in the module's own export list.
- Name collisions between imports are resolved during the merge phase: if two imports provide the same name with different general types, it is an error. If they provide the same name with the same general type but different language implementations, they are merged (implementation polymorphism).

## Import Side Effects

Importing a module causes its type declarations, fixity declarations, and typeclass definitions to become visible in the importing module. This is necessary for correct parsing (fixity affects precedence) and type checking (type aliases and typeclasses must be in scope).
