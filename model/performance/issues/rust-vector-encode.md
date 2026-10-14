# Rust clones every batch it binds out of a `Try`

Status: open. Parent: runtime/issues/encode-throughput.md. The larger cost
found with it, primitive vectors encoded one element at a time, is fixed:
600 MiB of `[Vector U8]` now writes in 0.73 s net (C++ 0.76) and reads in
0.40 s (C++ 0.27), against 4.68 s and 3.22 s before (lib/biolib/
runtime-bench, history.tsv label `stage1a-2`).

## Cause [read]

`Ok xs <- @next s` in a Rust pool becomes `match &n64 { Ok(b) =>
b.0.clone() }`: the compiler's Rust constructor-field projection
(library/Morloc/CodeGenerator/Pools/CAbi/Members/Rust.hs, `lcCtorField`)
borrows the subject and clones the field. C++ moves it
(`std::move(std::get<1>(n64.v)->f0)`), because it consumes the set of
projections the shared analysis `consumableProjectionLets`
(Grammars/Translator/Imperative.hs) marks as taking their payload. One
extra allocation and memcpy per batch, ~0.1-0.2 s per 600 MiB [inferred].

## Why the shared analysis cannot be reused as it is

Tried 2026-10-14 (render a consumable projection as `match subj { T::A(b)
=> b.into_inner().k, .. }`) and backed out after review. The analysis was
written for C++, where taking the payload leaves the subject usable;
Rust moves the whole subject, so valid programs stop compiling:

- a user-mapped `data` type (`data Rust => T = ...`) boxes its payload in
  `Box`, which has no stable `into_inner`;
- a tag test or `@show` of the subject evaluated after the projection on
  the same path (a second `match t` that does not project; `@throw (@show
  t)` in the arm) is a use after move: the analysis exempts tag tests and
  throw arms;
- a projection inside a `@try` body is a move inside an `Fn` closure: the
  analysis treats a do-block as transparent rather than as a capture.

## Solution

A Rust-specific decision: a mention of the subject evaluated after the
projection on the same path disqualifies it (mentions before it, or on
another branch, do not); a mention inside a do-block or closure counts as
a capture; a user box renders `(*b).k`. `RecBox::into_inner` moves when
the box is unshared and copies otherwise, so a wrong "consumable" costs a
copy, never data. The throw-arm exemption is also wrong for C++
(compiler/issues/codegen-ownership.md); fixing the shared rule positionally
would serve both.

Tests: test-suite/golden-tests/projection-consume (values only today);
add the three cases above as Rust goldens that must compile, and assert
that the generated Rust contains the move where it is allowed.

## Later

- Borrowed slices: decode `Vector a` to a type borrowed from the
  shared-memory block or mapping, checked by Rust lifetimes
  (stream-copies.md, design B). New native type mapping and an ownership
  change; model first.
- The size walk's fixed-width fast path covers Bool but not Enum (generated
  enum impls were not checked for `shm_size == width`).
- Voidstar byte order is unspecified (FMT-1 fixes only the packet header);
  matters for stream files moved between hosts.
