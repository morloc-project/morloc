# Streams

Stream types, file-backed streams, `@collect`, `@fold`. Prefix: STRM.

## File streams

### STRM-1 A stream's element type is fixed at open and checked against the file
Intent: proposed
Code: deviates (../runtime/issues/ifile-access.md)
Tests: spec-strm-1-1

`@open`, `@append`, `@stdin`, `@stdout` and `@stderr` take their element type
from an inline ascription. Opening a file whose recorded schema differs is an
`Err` at open time, before any element is read or written.

### STRM-2 A closed `OStream` reads back as the elements written, in order
Intent: proposed
Code: conforms 2026-10-21

Compression level, `@flush` and buffer size change sub-packet boundaries,
never the elements. An `IStream` yields them in order, one batch per `@next`,
and `Ok []` at the end.

### STRM-3 `IFile a` is a file holding one value of type `a`
Intent: ruled 2026-10-29
Code: conforms 2026-10-29
Tests: spec-strm-3-1

`IStream a` and `OStream a` are parameterized by the element type; `IFile`
is parameterized by the type of the whole value its file holds. A file
written as an `OStream a` holds `[a]` and opens as `IFile [a]`, the type
`@stream :: IFile [a] -> <IO> (IStream a)` takes; a file `@save`d from a
record `R` opens as `IFile R`. The file's stored schema must match `a`
(STRM-1); an `IFile Int` over a stream of `Int` is refused.

## Collect

### STRM-4 A streaming command has one stdout source on each path
Intent: ruled 2026-11-15
Tests: spec-strm-4-1, spec-strm-4-2, spec-strm-4-3, spec-strm-4-4, spec-strm-4-5, spec-strm-4-6, spec-strm-4-7, spec-strm-4-8, spec-strm-4-9, spec-strm-4-10, spec-strm-4-11, spec-strm-4-12, spec-strm-4-13, spec-strm-4-14
Code: conforms 2026-11-18

A stdout source is `@collect` or `@stdout`. Each path through a
`Collect`-returning command's body ends in exactly one, in tail position:
the last statement of a `do` block, an arm of `?` or `match`, the body of a
`let`, or the body of a leading or applied lambda; never a `where` binding.
A path that ends in `@throw`, or in a throw the compiler adds, needs none. A
source anywhere else is a build error. Its output is every batch its sink
receives, or every `@write` to its handle, in order.

Why: stdout is one typed stream per run, and `-f`, `-z`, `-o` and the
output actions each configure that one output.

### STRM-7 `Collect a` is the output a streaming command writes
Intent: ruled 2026-11-15
Tests: spec-strm-7-1, spec-strm-7-2, spec-strm-7-3, spec-strm-7-4, spec-strm-7-5, spec-strm-7-6, spec-strm-7-7, spec-strm-7-8, spec-strm-7-9, spec-strm-7-10, spec-strm-7-11
Code: conforms 2026-11-18

`@collect :: (([a] -> <IO,e> ()) -> <IO,e> ()) -> <IO,e> (Collect a)`.
`Collect a` stands for the `[a]` of every batch, in order, before any
output action: a whole-list handler receives that `[a]`, a fold step its
batches, and with no action it is what stdout gets. Only `@collect` and
`@stdout` produce one; the typechecker gives `Collect` no rule of its own,
so the arms of a streaming body agree through unification and an arm of
type `()` is a type error. A function whose signature returns `Collect` is
a command: morloc code never names it, and its signature names a concrete
`a`. `Collect` appears in no source, instance, class or local signature, no
type declaration names it, no instance method uses `@collect` or
`@stdout`, and no output-action handler returns it. Outside morloc code a streaming
command's output has type `[a]`: help, `--json-help` and the API show
that.

### STRM-8 `@stdout` hands a body the stdout stream
Intent: ruled 2026-11-15
Tests: spec-strm-8-1, spec-strm-8-2, spec-strm-8-3, spec-strm-8-4, spec-strm-8-5, spec-strm-8-6, spec-strm-8-7, spec-strm-8-8
Code: conforms 2026-11-20

`@stdout :: (OStream a -> <IO,e> b) -> <IO,e> (Collect a)` opens stdout,
runs the body on its handle and closes it. `@collect p` is
`@stdout (\o -> p (\xs -> @write 0 o xs))`. The body may pass the handle
on. `@close` applied to the bracket's parameter is a build error; a handle
closed any other way (through a helper or another name) fails the run.
A process the body forks (os.fork, multiprocessing, mclapply, fork())
writes the handle as the body does, each batch whole, in the order the
writes happen (ruled 2026-11-20); only the bracket closes it, and a child
that returns from the call it was forked in ends there (FORK-17).

### STRM-6 A batch handed to a sink is not changed afterwards
Intent: proposed
Code: conforms 2026-11-25

A producer that hands a batch to its sink does not change that batch, or
any value inside it, afterwards. A whole-list gather may keep the batch
itself rather than a copy, so a producer that refills one buffer and hands
it on each time would gather that buffer's last contents many times over.

### STRM-9 A put that raises fails its gather or fold
Intent: ruled 2026-11-15
Code: deviates 2026-11-20 (a batch that fails to encode while crossing to a sink in another pool marks nothing; issues/cli-directives.md)
Tests: spec-strm-9-1

A put into a `@collect` sink that raises (a batch that does not encode, a
write that fails, a fold step that throws, an interrupt) fails the
`@collect`, and so the command, even when the producer catches the error
and returns normally. A batch the producer put is either in the result or
the command fails; nothing is lost silently. A put counts from the moment
it is made until the `@collect` takes its result: a put still running
after that, from a thread or process the producer let outlive its call,
fails as a late put and is not part of the result.

### STRM-5 What `@fold` promises when `@init` and `@combine` break their laws
Intent: ruled 2026-11-20
Code: conforms 2026-11-20
Tests: spec-strm-5-1

`@fold init step combine` promises the sequential fold `foldl step init xs`
over the batches `xs` the producer put, for any split of the batches across
threads, forked children and the side replay, provided:
- identity: combine init a == a == combine a init;
- associativity: combine a (combine b c) == combine (combine a b) c;
- commutativity, when the producer's puts can run concurrently or it forks:
  combine a b == combine b a;
- homomorphism over splits: for every xs = ys ++ zs,
  combine (foldl step init ys) (foldl step init zs) == foldl step init xs.
These are the user's obligation; nothing checks them, and a fold that
breaks one may answer differently with the thread split. A step may
mutate its accumulator: every slot starts from its own copy of init
(FOLD-6).
