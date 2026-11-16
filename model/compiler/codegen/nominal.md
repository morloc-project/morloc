# Nominal types (NOM)

How code generation identifies a declared record, object, table or `data`
type. Serves GEN-1, GEN-9 and GEN-14 (`../../language/generated-types.md`).

### NOM-1 A declared type is identified by its key
Intent: proposed
Code: deviates (issues/nominal.md)

A record, object, table or `data` type in a native or unpacked type is an
instance of its declaration. Its key is the declaration's name, its
resolved arguments and its level (native or unpacked). Two occurrences are
one type exactly when their keys are equal. No identity question reads
form text, rendered native text or an occurrence's fields.

The arguments of a key are native-level types in which every declared type
is itself a key, so a key is finite and compared by value. An instance whose
arguments contain the instance itself (`type X = Rose ?X`, GEN-9, for a
record or a `data` type; GEN-9 rules the `data` case) refers to itself
there by a self-argument, an
index naming the enclosing key, so `Rose ?X` is `Rose ?(self 0)`, and two
spellings of one such type are one key. Its native name is still derived
from its written arguments (GEN-9). A key reached through spellings that
give two different names is a compile error, never two native types.

Anonymous records and the rows at `Rec` parameters are not declared types.
They are compared structurally, field order included. Recursive type
aliases are not declared types either. They keep a cut of their own.

### NOM-2 Every occurrence is its definition at its key
Intent: proposed
Code: deviates (issues/nominal.md)

Each declared type has one definition per language. The fields or arms of
every occurrence are the definition's at its key's arguments, so an
occurrence cannot disagree with its definition.

### NOM-3 Arguments come from the general type
Intent: proposed
Code: deviates (issues/nominal.md)

A key's arguments are resolved from the general type's arguments, at every
nesting of declared types in them. They are never paired with a concrete
form or read back from fields.

### NOM-4 An unpacked key is identified by the key it unpacks
Intent: proposed
Code: deviates (issues/nominal.md)

The unpacked form of a declared type is identified by the native-level key
it unpacks: two unpacked keys are equal exactly when they unpack equal
native-level keys. The declaration at the unpacked types of that key's
arguments is computed from it (`Pair Date` unpacks to `Pair` at `Str`),
not part of its identity. So `Pair Date` and `Pair Str` have distinct
unpacked keys, and a general name (`@typeof`, re-inference) is read from
the native-level key.

### NOM-5 The spelling groups keys that are one native type
Intent: proposed
Code: deviates (issues/nominal.md)

A key's spelling is its declaration at the arguments its native form
spells. Phantom, `Nat`, `Str` and `Rec` parameters are erased, and morloc
types that are one native type (`Int` and `I64` in C++) have one spelling.
Keys with one spelling are one declaration in a pool. If they convert a
field differently, that is the compile error of GEN-14. Native identity is
never decided by comparing rendered text.

### NOM-6 A self-reference is a reference to a key on the path
Intent: proposed
Code: deviates (issues/nominal.md)

A declared type that holds itself refers to itself, inside itself, by a
back-reference that carries its key. It has no other back-reference shape.
A reference to the same declaration at another key is that other key's
type. It is expanded, and cut where its own key is on the path.

### NOM-7 Schema text does not change
Intent: proposed
Code: conforms 2026-11-16

The wire schema a pool writes for a type is byte-identical to the schema
written before keys were introduced. That covers declaration names,
`#k` suffixes and back-reference names. Stored files carry their schema,
and the runtime compares declaration names when it reopens, loads,
appends to or concatenates them, so a change of text is a compatibility
break.
