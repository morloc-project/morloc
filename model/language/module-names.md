# Module names

Modules, imports, exports, and which declaration a name refers to across modules. Prefix: MOD.

## Modules

### MOD-1 A file holds one or more modules
Intent: proposed
Code: unaudited

Each `module` keyword starts a new module, which runs until the next
`module` keyword or the end of the file. A module body may be empty.

### MOD-2 An export list names exactly what a module exports
Intent: proposed
Code: unaudited

`module m (a, b)` exports `a` and `b`. `module m ()` exports no terms.
Exporting a name the module has no definition for is a load error.

### MOD-3 `(*)` exports every term defined in the module, imported ones included
Intent: ruled 2026-10-04
Tests: spec-mod-3-1
Code: conforms 2026-10-07

A term is defined in a module if it is declared there or imported into it.
A module that imports `foo` and `bar` and exports `(*)` is their union.

Why: limiting `*` to terms written in the module was rejected; a module that
should expose only some terms uses an explicit list.

### MOD-4 An anonymous module takes its name from the import that reaches it
Intent: proposed
Code: unaudited

`module (*)` in `utils.loc`, reached by `import .utils`, is named `utils`.

### MOD-5 What makes two imports the same module
Intent: open
Code: unaudited

When do two imports name one module rather than two? The cases that
need an answer:

- the same directory, imported locally as `.lib` by one file and installed
  as `lib` for another;
- an anonymous module reached by two different import paths;
- one file reached by two dotted paths through a symlink.

The answer fixes the module identity that MOD-14 and MOD-17 rely on.

## Imports

### MOD-6 A dotted import resolves from the project root
Intent: proposed
Code: unaudited

The project root is the directory of the entry file given to the compiler.
`import .a.b` names `a/b.loc` or `a/b/main.loc` under that root. The
same import line names the same module in every file of the project.

### MOD-7 A dotless import is a system module
Intent: proposed
Code: unaudited

`import m` names the installed module `m`, and `import .m` names a local
one. A local module and a system module never collide.

### MOD-8 Both a file module and a directory module exist
Intent: open
Code: unaudited

If both `a/b.loc` and `a/b/main.loc` exist, `import .a.b` either:

- (a) picks `a/b.loc`, which is what the manual says today; or
- (b) is a load error naming both files.

### MOD-9 Selectors limit what an import brings into scope
Intent: proposed
Code: unaudited

Without a selector, `import m` brings every term `m` exports into scope.
`import m (a, b)` brings only `a` and `b`, and naming a term that `m`
does not export is a load error.

### MOD-10 Methods are not imported by name
Intent: proposed
Code: unaudited

A typeclass method has no identity apart from its class. If `x` is a method,
`import m (x)` is a load error, and the message names the class.

### MOD-11 Instances follow the module, not the export list
Intent: open
Code: unaudited

Importing a module brings all of its instances into scope, whatever the
selector, and an export list of `()` still exports them. Open: are instances
visible transitively? If `a` imports `b` and `b` imports `c`, does `a` see
`c`'s instances?

### MOD-12 One term name imported from two modules
Intent: open
Code: unaudited

If `import a` and `import b` both bring `f` into scope, is `f` one term
with two implementations (as with `root-py` and `root-cpp`), or is it a
conflict? If it is one term, what must agree between the two declarations:
the general type, the class constraints, the docstrings?

### MOD-13 A `source` path resolves against the file that names it
Intent: proposed
Code: unaudited

A module moves together with the native files it sources.

## Types

### MOD-14 A type name refers to the declaration in scope where it is written
Intent: ruled 2026-10-04
Tests: spec-mod-14-1, spec-mod-14-2
Code: conforms 2026-10-07

A type name in a signature keeps the meaning it has in the module that
wrote the signature. Importing the term does not rebind the name in the
importer's scope. Nothing resolves a type name in a global scope.

### MOD-15 An undeclared type name is a load error
Intent: ruled 2026-10-04
Tests: spec-mod-15-1, spec-mod-15-2
Code: conforms 2026-10-07

A type that exists only as native types in some languages still needs a
general declaration (`newtype X`).

### MOD-16 Importing a type and declaring the same name is a load error
Intent: ruled 2026-10-04
Tests: spec-mod-16-1, spec-mod-16-2
Code: conforms 2026-10-07

`import m (P)` together with a local declaration of `P` is rejected. Without
the import, the local `P` is legal and shadows nothing.

### MOD-17 Same-named types are told apart by module only where they overlap
Intent: ruled 2026-10-04
Code: deviates #147

Several types with one name may coexist in a program. Help text, error
messages and schemas qualify a type with its module (`lib.P`) exactly where
two types in the program share the name, and nowhere else.

### MOD-18 Built-in type names are ordinary declarations
Intent: ruled 2026-10-04
Code: unaudited

`Int`, `Str` and the other base names have no reserved meaning to the name
system. A module other than the standard library may declare them, which
allows an alternative prelude.
