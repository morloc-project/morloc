# Generated native types

Native definitions the compiler writes for records and `data` types: kinds,
public names, the files that hold them and how programs reach them.
Prefix: GEN. Plan: /work/plans/generated-native-types/PLAN.md.

### GEN-1 Every record and `data` type has a native form in every language
Intent: ruled 2026-11-14
Tests: spec-gen-1-1, spec-gen-1-2
Code: unaudited

A type's form in language `L` is the one its program declares
(`record L => R = ...`, `data L => D = ...`), or else `L`'s default kind.
A declared form is either a string, a type the user writes (NATIVE-1), or a
bare word, a kind whose definition the compiler generates. No program fails
for want of a per-language line.

A record that refers to itself, anywhere in its fields, is generated in C++
and Rust as a handle to its shared, immutable fields (ruled 2026-11-15):
C++ as CPP-6 describes, Rust `pub struct R(pub Arc<RFields>)`, read through
`Deref` (`r.k`). Its `?R` is the language's ordinary optional of `R`, so a
field read borrows and a copy shares the node, and generic code sees one
spelling of `?a`. In Rust a node is changed in place only while no other
handle shares it (`Arc::make_mut`), and dropping a handle releases the `?R`
and `[R]` fields below it in bounded stack. Records that refer to each other
are refused unless a `data` type cuts the cycle (SUM-4).

### GEN-2 A kind is a word the language defines
Intent: ruled 2026-11-14
Tests: spec-gen-2-1
Code: unaudited

Each language lists its record kinds and its `data` kinds, and one default
of each, in its descriptor. A kind word the language does not list is a
compile error that names the language and lists its kinds. A descriptor
whose kinds are malformed is refused when it is loaded, naming the file and
the key.

Kinds: C++ `struct` (records) and `variant` (`data`); Rust `struct` and
`enum`; Python `dict` (default), `dataclass` and `pydantic` for records,
`dataclass` (default) and `pydantic` for `data`; R `list`.

### GEN-3 The legacy container strings mean the default kind
Intent: ruled 2026-11-14
Tests: spec-gen-3-1
Code: unaudited

`record Py => R = "dict"`, `"OrderedDict"`, `record R => R = "list"` and
`record Cpp => R = "struct"` / `record Rust => R = "struct"` mean the
language's default record kind, with a deprecation warning at the line.

### GEN-4 A generated type's name is derived from its declaring module
Intent: ruled 2026-11-14
Code: unaudited

The public name of a generated type `T` declared in module `m1.m2-x` is a
fixed prefix, then the module's components, then `T`: C++
`mlc::types::m1::m2_x::T`, Rust `crate::mlc::types::m1::m2_x::T`, Python
`mlc_types.m1.m2_x.T`. Components split at `.`, and `-` becomes `_`. A
local submodule of a package (`import .lib.x` inside package `p`) is
placed under the package's name (`mlc::types::p::lib::x::T`), whether the
package is being developed or is installed. A module that belongs to no
package (a script with no `package.yaml`, its local imports) is placed
under the component `_local`. No override exists. The name is the same in
every program that declares `T` in the same module.

Why: 2026-11-15, the `_local` placement was narrowed to modules of no
package, so that a package's own sourced files name its types the same
way before and after it is installed.

### GEN-5 Colliding or unspellable generated names are compile errors
Intent: proposed
Tests: spec-gen-5-1, spec-gen-5-2
Code: unaudited

A build fails, naming both declarations, when two generated types get one
public name in a language. It fails, naming the type and the word, when a
module component, type name or field name cannot be spelled in a language
that generates the type (a keyword with no escape there).

### GEN-6 The definitions are written to files the pool reads first
Intent: ruled 2026-11-14
Tests: spec-gen-6-1, spec-gen-6-2
Code: unaudited

For every module `M` and language `L` with a generated type the program
uses in `L`, the build writes one file under the build directory:
`pools/types/cpp/mlc/types/<path>.hpp`,
`pools/types/rust/mlc/types/<path>.rs`, `pools/types/py/mlc_types/<path>/__init__.py`. The pool includes it before any
sourced file, so a sourced file may name the type. A sourced C++ or
Python file may also import the file itself, by the path above relative
to `pools/types/<lang>`. In Rust every sourced file is part of the crate
root, which also offers each generated `data` type by its bare name; a
sourced file names a type by its path or by that name, and two sourced
files that both `use` the same name clash. Its contents are a function of
the program's closure, and it includes or imports only what its
definitions name. A generated Rust `data` type holds each constructor's fields
in a generated arm type, `D_C` for constructor `C` of `D`: an alias of the
crate's generated node type `crate::mlc::Node<(fields...)>`, a standard
`Arc` read through `Deref` and built with `D_C::new` or `.into()`. A type
file names no morloc type; the crate's `mlc` module, which the pool or a
standalone crate supplies, holds the node and the release function its
drops call.

### GEN-7 A generated file imports what its fields name
Intent: ruled 2026-11-14
Code: unaudited

A field's native form names a namespace root (`std`, `Eigen`, `numpy`).
The root is another generated file's, a builtin of the language, or the
`namespace` of a dependency the program's `package.yaml` files declare,
whose `import` the file then imports.

### GEN-8 A generated record in a compiled language holds host values
Intent: ruled 2026-11-14
Tests: spec-gen-8-1, spec-gen-8-2
Code: conforms 2026-11-15

In C++ and Rust a field of a generated record is in the host's convention,
as a field of a user-written struct is: a function field whose result is an
effect runs the effect when called. A field that is itself a suspension
stays a callable of no arguments. Morloc code that builds such a record or
reads such a field adapts the function where it does so, so in morloc the
field keeps its declared type (EFF-2).

### GEN-9 A generated definition follows its declaration
Intent: proposed
Code: unaudited

A generated record or `data` type has the declaration's type parameters as
its own (C++ template parameters, Rust generic parameters) and each field
the form of the declared field type. Whether a program uses `Pair Int`,
`Pair Str` or both, `Pair a` has one definition.

### GEN-10 A generated type that cannot be written alone stays in the pool
Intent: open
Code: unaudited

A generated type whose field names a type no generated file can import
(a user-written type from a sourced file, a type with no dependency
namespace), or whose fields differ between uses other than by its
parameters, is defined only inside the pool, after the sourced files, and
is absent from the generated file. Open: is that the rule, or is such a
program refused?

### GEN-11 A Python class kind holds its fields as attributes
Intent: proposed
Tests: spec-gen-11-1, spec-gen-11-3, spec-gen-11-4, spec-gen-11-5, spec-gen-11-6, spec-gen-11-7
Code: deviates (issues/values.md: a user-written class form)

A record of kind `dataclass` is an instance of a frozen, slotted dataclass;
of kind `pydantic`, an instance of a frozen pydantic v2 model built without
validation. Its fields are read as attributes, and also by subscript
(`p["name"]`), so code written for the dict kind keeps reading them. A value
sent from Python must be an instance of exactly the generated class. Only
the package declaring a record, or that package's implementation module
for Python (`<package>-py`), may give it a class kind; the modules of no
package (a script and its local imports) count as one package. A record
has at most one class kind in a language. A user-written Python form
(`record Py => R = "Cls"`) is constructed by calling the class with the
fields as keywords and read by attribute.

### GEN-12 A Python `data` value is an instance of its arm's class
Intent: proposed
Code: unaudited

Each constructor `C` of a `data` type `D` is a frozen dataclass `C`,
subclass of an empty base class `D`, with positional fields `f0 ... fn`.
A nullary constructor is a class with no fields.

### GEN-13 `morloc types` writes the generated files anywhere
Intent: ruled 2026-11-14
Tests: spec-gen-13-1
Code: deviates (issues/records.md)

`morloc types FILE.loc -o DIR [--lang L]` writes the files GEN-6 names for
the program, under `DIR`, for programs outside morloc: `DIR/cpp` a header
tree (`-I DIR/cpp`), `DIR/py` a package root (`mlc_types`), and `DIR/rust`
a crate of its own (`Cargo.toml`, `src/lib.rs`) with no dependency. Each
language the program generates types in gets its directory, replaced whole;
`--lang` writes one, and a language with no generated types is an error.
Outside a pool a recursive value is released as the language releases it,
without the pool's bounded-stack drain (STATE-8).
