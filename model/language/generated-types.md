# Generated native types

Native definitions the compiler writes for records and `data` types: kinds,
public names, the files that hold them and how programs reach them.
Prefix: GEN. Plan: /work/plans/generated-native-types/PLAN.md.

### GEN-1 Every record and `data` type has a native form in every language
Intent: ruled 2026-11-14
Tests: spec-gen-1-1, spec-gen-1-2, spec-gen-1-3
Code: deviates (issues/records.md)

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
Code: deviates (issues/records.md)

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
Code: conforms 2026-11-18

`record Py => R = "dict"`, `"OrderedDict"`, `record R => R = "list"` and
`record Cpp => R = "struct"` / `record Rust => R = "struct"` mean the
language's default record kind, with a deprecation warning at the line.

### GEN-4 A generated type's name is derived from its declaring module
Intent: ruled 2026-11-14
Tests: spec-gen-4-1, spec-gen-4-2, spec-gen-4-3, spec-gen-4-4
Code: conforms 2026-11-18

The public name of a generated type `T` declared in module `m1.m2-x` is a
fixed prefix, then the module's components, then `T`: C++
`mlc::types::m1::m2_x::T`, Rust `crate::mlc::types::m1::m2_x::T`, Python
`mlc_types.m1.m2_x.T`. Components split at `.`, and `-` becomes `_`. A
local submodule of a package (`import .lib.x` inside package `p`) is
placed under the package's name (`mlc::types::p::lib::x::T`), whether the
package is being developed or is installed. A module that belongs to no
package (a script with no `package.yaml`, its local imports) is placed
under the component `_local`; so is a package that names no library
(PKG-1). A module of a library `l` other than `std` (`l/m1.m2-x`, PKG-1)
is placed under the components `_lib` and `l`
(`mlc::types::_lib::l::m1::m2_x::T`); no name produces a component that
starts with `_`. No override exists. The name is the same in
every program that declares `T` in the same module.

Why: 2026-11-15, the `_local` placement was narrowed to modules of no
package, so that a package's own sourced files name its types the same
way before and after it is installed.

### GEN-5 Colliding or unspellable generated names are compile errors
Intent: proposed
Tests: spec-gen-5-1, spec-gen-5-2, spec-gen-5-3
Code: deviates (issues/records.md)

A build fails, naming both declarations, when two generated types get one
public name in a language. It fails, naming the type and the word, when a
module component, type name or field name cannot be spelled in a language
that generates the type (a keyword with no escape there).

### GEN-6 The definitions are written to files the pool reads first
Intent: ruled 2026-11-14
Tests: spec-gen-6-1, spec-gen-6-2
Code: deviates (issues/records.md)

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
Tests: spec-gen-7-1, spec-gen-7-2, spec-gen-7-3
Code: conforms 2026-11-18

A field's native form names a namespace root (`std`, `Eigen`, `numpy`).
The root is another generated file's, a builtin of the language, or the
`namespace` of a dependency the program's `package.yaml` files declare,
whose `import` the file then imports. A `cpp-deps` entry gives both keys
(at least one header) or neither; a `rust-deps` entry's namespace defaults
to the crate's name with `-` as `_`, and its import is the crate itself,
which a crate built from the generated files outside morloc lists among its
dependencies. A namespace is one identifier, not one the language or
morloc owns (`std`, `mlc`), and no two dependencies of a language claim
one. Python annotates a field of a dependency's type as any value, so its
files import none. A type whose field names a root none of these resolve
stays in the pool (GEN-10).

### GEN-8 A generated record in a compiled language holds host values
Intent: ruled 2026-11-14
Tests: spec-gen-8-1, spec-gen-8-2
Code: deviates (issues/records.md)

In C++ and Rust a field of a generated record is in the host's convention,
as a field of a user-written struct is: a function field whose result is an
effect runs the effect when called. A field that is itself a suspension
stays a callable of no arguments. Morloc code that builds such a record or
reads such a field adapts the function where it does so, so in morloc the
field keeps its declared type (EFF-2).

### GEN-9 A generated definition follows its declaration
Intent: ruled 2026-11-16
Tests: spec-gen-9-1, spec-gen-9-2, spec-gen-9-3, spec-gen-9-4, spec-gen-9-5, spec-gen-9-6, spec-gen-9-7, spec-gen-9-8, spec-gen-9-9, spec-gen-9-10, spec-gen-9-11
Code: deviates (issues/records.md)

A generated record or `data` type has the declaration's type parameters as
its own (C++ template parameters, Rust generic parameters), each spelled
`T_<param>`, and each field the form of the declared field type. Whether a
program uses `Pair Int`, `Pair Str` or both, `Pair a` has one definition,
`template <class T_a> struct Pair` / `pub struct Pair<T_a>`, which sources
name at their arguments (`Pair<int64_t>`).

A use's arguments are its general arguments in their native forms on the
native side, and in their unpacked forms on the wire side of a packed
argument (`Pair Date` is `Pair<int64_t>` natively and is sent as
`Pair<std::string>`). Its fields are the definition's at those arguments;
a use whose fields do not render as the definition does at those arguments
is a compile error naming the record, the field and both renderings. A
`data` type's arguments are found the same way, so its native and wire
forms are distinct instances (`Choice<int64_t>` and `Choice<std::string>`
for a packed `Choice Date`). A `data` instance whose
arguments contain the type itself other than through a record
(`type X = Rose ?X`) has no finite spelling at them; it is a type of its
own in the pool, named by its arguments.

A parameter no field holds natively, and a parameter of another kind
(`Nat`, `Str`, `Rec`), is erased: `Grid2 3` and `Grid2 4` are one native
type. A record with a packed field holds the field's native form. A
recursive type used at an argument that needs conversion is refused.

Why: one nameable definition per declared type, the same in every program
and every pool, is what a source and a program outside morloc can be
written against.

### GEN-10 A generated type that cannot be written alone stays in the pool
Intent: ruled 2026-11-15
Tests: spec-gen-10-1
Code: conforms 2026-11-16

A generated type whose field names a type no generated file can import (a
user-written type from a sourced file, a type with no dependency
namespace), or whose field holds such a type, or that has no module to be
written in (a record named like a type the compiler defines), is defined
only inside the pool, after the sourced files, in a pool-private namespace
(C++ `mlc_pool::`, Rust `crate::mlc_pool`). It is absent from the generated
file and from `morloc types`, which says so; sources cannot name it.

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
has at most one class kind in a language. A user-written Python class
form (`record Py => R = "Cls"`) is a future feature (ruled 2026-11-18):
until then such a record is a dict.

### GEN-12 A Python `data` value is a member or an instance of its type's class
Intent: ruled 2026-11-18
Tests: spec-gen-12-1
Code: conforms 2026-11-18

A `data` type whose constructors take no arguments is an `enum.Enum`
subclass named for the type, with one member per constructor, named for it,
whose value is its tag (its position in the declaration): `Base.A`. Any
other `data` type `D` is a class `D` whose constructors are attributes of
it: a constructor `C` with fields is a frozen, slotted dataclass `D.C`,
subclass of `D`, with positional fields `f0 ... fn`; one with no fields is
a single instance `D.C` of such a class with no fields. A value received
or built is the member or an instance of exactly its constructor's class,
and a value sent must be one: no other form is accepted. The classes live
in the generated file of the module declaring the type (GEN-6), for every
such type the program's Python code can use; a type the compiler itself
declares (`Try`) lives under `mlc_types._builtin`. A constructor named like
a Python keyword is the attribute with a trailing underscore (`Opt.None_`).
A newtype over a `data` type holds that type's value, and a Python form
written for either is not used (with a warning).

### GEN-13 `morloc types` writes the generated files anywhere
Intent: ruled 2026-11-14
Tests: spec-gen-13-1
Code: conforms 2026-11-16

`morloc types FILE.loc -o DIR [--lang L]` writes the files GEN-6 names for
the program, under `DIR`, for programs outside morloc: `DIR/cpp` a header
tree (`-I DIR/cpp`), `DIR/py` a package root (`mlc_types`), and `DIR/rust`
a crate of its own (`Cargo.toml`, `src/lib.rs`) with no dependency. Each
language the program generates types in gets its directory, replaced whole;
`--lang` writes one, and a language with no generated types is an error.
Outside a pool a recursive value is released as the language releases it,
without the pool's bounded-stack drain (STATE-8).

### GEN-14 A generated record or a `data` type converts its own fields, once per write
Intent: ruled 2026-11-17
Tests: spec-gen-14-1, spec-gen-14-2, spec-gen-14-3, spec-gen-14-4, spec-gen-14-5, spec-gen-14-6, spec-gen-14-7, spec-gen-14-8, spec-gen-14-9, spec-gen-14-10, spec-gen-14-11, spec-gen-14-12
Code: conforms 2026-11-17

In C++ and Rust a generated record, and every `data` type, generated or
user-mapped, is marshalled as it is held: its node converts each field
(of a record) or arm field (of a `data` type) that is not written as it
is held (a packed value, a closure) outside any record it holds, which
converts its own. The value
that sizing a write converts is the value written, so a packer runs once
per field per write, and a packer that returns values of different sizes
cannot overrun the space sized for them. A field whose wire form has a
fixed size, or is an integer held inline, sizes the same whatever it holds,
so it is converted only where it is written, and its packer fails, if it
does, in the write. Where a field's conversion
depends on the record's arguments, each instance converts as its use says;
two morloc types that are one native type and convert a field differently
are a compile error naming the type. A packer that fails during a write
fails the write as any packer failure does. A value holding a generated
record is written as it is held wherever the pool writes it: a call, a
stream, a gather or fold, @save and @load, and so is one the pool only
holds. A recursive value is marshalled by one walk that re-enters its
cycle, so a packed value inside the cycle is converted at every level only
where such a node holds it; a recursive type with any other packed value in
its cycle (in Python or R, or in a user-mapped record) is refused wherever a
value of it would be marshalled. The form `struct` names a
record the compiler generates; a language with no such kind takes no
written type of that name.

Why: converting in the node copies nothing the field does not need, and
converting once is what makes the size of a write the size written.
