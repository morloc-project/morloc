# Module names

Modules, imports, exports, and which declaration a name refers to across modules. Prefix: MOD.

## Modules

### MOD-1 A file holds one or more modules
Intent: ruled 2026-11-15
Tests: spec-mod-1-1, spec-mod-1-2, spec-mod-1-3, spec-mod-1-4, spec-mod-1-5, spec-mod-1-6
Code: conforms 2026-11-18

Each `module` keyword starts a new module, which runs until the next
`module` keyword or the end of the file. A module body may be empty. The
module an import reaches is the one whose name is the import's last
component, or the file's anonymous module. Every other module in the file
is a local module of the file's package: another module of the file
imports it as `.foo` (inside package `k`, its key is `k.foo`). A sibling
module and a local file of the same name are a load error.

### MOD-2 An export list names exactly what a module exports
Intent: proposed
Code: conforms 2026-10-21

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
Code: conforms 2026-10-21

`module (*)` in `utils.loc`, reached by `import .utils`, is named `utils`.

### MOD-5 Two imports name one module iff their canonical keys are equal
Intent: ruled 2026-11-15
Tests: spec-mod-5-1, spec-mod-5-2, spec-mod-5-3, spec-mod-5-4, spec-mod-5-5
Code: conforms 2026-11-18

Every module has one canonical key: `x` or `x.s` for a standard library module and
its submodules, `l/x` or `l/x.s` for a module of library `l` (PKG-1), `k.a`
for the local module `.a` of an installed package with key `k`, and `.a`
for a local module of the program. Two imports name the same module iff
their keys are equal. A module's header never becomes its key: a header
that does not match the key the import gives is a load error, and so is
one file reached under two keys.

Why: keying a module by its header let a local file declaring `module
root` replace the stdlib `root`.

## Imports

### MOD-6 A dotted import resolves from its package's root
Intent: ruled 2026-10-19
Tests: spec-mod-6-1
Code: conforms 2026-10-19

A module belongs to a package: the program, whose root is the directory of
the entry file given to the compiler, or a package reached by a dotless
import, whose root is the directory of that package's file. A local module
belongs to the package of the module that imports it. `import .a.b` names
`a/b.loc` or `a/b/main.loc` under the root of the importing module's
package, so a package's local imports mean the same wherever it is
imported. In a package `p` other than the program, the module it names is
`p.a.b`, distinct from any other package's `.a.b`.

Why: a package's own files are relative to it; resolving them from the
program that imports it made an installed package with submodules
unloadable.

### MOD-7 A dotless import is an installed module
Intent: ruled 2026-11-15
Tests: spec-mod-7-1, spec-mod-7-2, spec-mod-7-3, spec-mod-7-4, spec-mod-7-5, spec-mod-7-6, spec-mod-7-7, spec-mod-7-8
Code: conforms 2026-11-18

`import m` names the standard library module `m`, `import l/m` names module `m` of
library `l` (PKG-1), and `import .m` names a local one. Each resolves to
exactly one place in the plane (PKG-3); there is no fallback to any other
path. `import std/m` is a load error: standard library modules have one
spelling.
A local module and an installed module never collide: a dotless import
never reaches a local file. `morloc make` installs a missing module,
unless the importing module's package has a local module of that name. A
dotless import with no installed module is then a load error; when a
local module of that name exists, the error names it and says to import it
as `.m`.

### MOD-8 Both a file module and a directory module exist
Intent: open
Code: unaudited

If both `a/b.loc` and `a/b/main.loc` exist, `import .a.b` either:

- (a) picks `a/b.loc`, which is what the manual says today; or
- (b) is a load error naming both files.

### MOD-9 Selectors limit what an import brings into scope
Intent: proposed
Code: conforms 2026-10-21

Without a selector, `import m` brings every term `m` exports into scope.
`import m (a, b)` brings only `a` and `b`, and naming a term that `m`
does not export is a load error.

### MOD-10 Methods are not imported by name
Intent: proposed
Code: conforms 2026-10-21

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
Intent: ruled 2026-11-20
Tests: spec-mod-12-1, spec-mod-12-2
Code: conforms 2026-11-20

Two imported terms with different signatures cannot both be imported
unqualified under the same name: the importing module is refused, whether
or not it uses the name. Import one of them qualified (`import map as m`)
or select names (`import map (Map)`). Imports of one term, as from a
module and its language implementations (`root`, `root-py`), are not a
clash.

### MOD-13 A `source` path resolves against the file that names it
Intent: proposed
Code: conforms 2026-10-21

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
Code: conforms 2026-10-21

`Int`, `Str` and the other base names have no reserved meaning to the name
system. A module other than the standard library may declare them, which
allows an alternative prelude.
