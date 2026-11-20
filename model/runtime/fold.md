# Native fold accumulators

A `@fold` keeps its accumulator native in the pool where `step` runs. The
bracket makes a fork-side entry (SIDE) whose native pointer is the
runtime's slot table (morloc-runtime fold.rs): a list of slots, one per
put that was ever in flight at the same time as the others, at most
MAX_SLOTS. A slot is FREE (it holds a value), BUSY (a put has it checked
out, and the slot records that put's thread) or FAILED (a step on it
threw). No slot belongs to a thread. Values are the binding's own
pointers, which the runtime never reads. The entry lock guards the native
pointer and the slot table; no user code runs under it. Each put carries
the fold's step itself, so nothing a put runs belongs to the bracket's
frame. A forked child's put goes to the side file, as for a gather
(SIDE-4, SIDE-10).

### FOLD-1 A fold put checks its slot out, steps it unlocked, and stores it back
Status: implemented
Checked by: a_put_while_another_steps_folds_into_its_own_fresh_slot, a_store_after_the_seal_fails_and_the_fold_fails, a_dropped_fold_returns_its_values_and_refuses_puts, golden:fold-native, golden:collect-fold-cpp, tla:ForkSide_fold, tla:ForkSide_fold_store_ignores_seal.bug, tla:ForkSide_fold_append_ignores_taken.bug

A put in the entry's owner (SIDE-4) has three phases. Under the entry
lock: if the native pointer is null the put fails late; a FAILED slot
fails the put (FOLD-4), and so does a BUSY slot the calling thread holds
(FOLD-5); otherwise it takes the lowest FREE slot, or makes one when none
is FREE (FOLD-6), moves the value out and marks the slot BUSY, held by
the calling thread. With no lock
held, it runs `step value batch`; the value is exclusive to the thread, so
a step may mutate it in place. Under the entry lock again: if the native
pointer is null the put fails late and drops the value after unlocking;
otherwise it stores the new value into the BUSY slot its thread holds,
marks it FREE, and only then
returns success. So no put returns success once take has sealed the entry
(FOLD-2). A store that skips the null check returns success for a batch
the result may already have been combined without.

The third phase finds the entry again by its id, never through a pointer
kept from the first: take may have returned and dropped the entry while
the step ran (FOLD-10). A put with a stream handle writes and flushes the
batch instead (FOLD-9).

In the owner the put checks out natively in every state but TAKEN, SIDE
and FAILED included: only a child's batches need the side file, and they
need commutativity anyway (STRM-5). A null native pointer in the owner is a late
put, never a side write; the runtime's `with_native` reports the two
outcomes apart.

### FOLD-2 Take seals the entry before it counts the slots
Status: implemented
Checked by: a_thread_that_puts_while_take_starts_is_counted_or_late, tla:ForkSide_fold, tla:ForkSide_fold_slot_after_take.bug

Take in the owner CASes the state word to TAKEN (waiting out SWITCHING,
SIDE-1) and nulls the native pointer under the entry lock in the same
step: that is the seal. The slots it combines are the slots that exist at
the seal; no slot is made after it, since a put makes a slot only under
the lock with the pointer non-null. A take that counts the slots and
seals later misses a slot a new thread made and stored in between, and
that thread's returned put is lost.

A take on FAILED seals too, then fails (SIDE-8): an owner put keeps no
returned success for an entry whose fold failed.

### FOLD-3 Take replays the side records through step, each once
Status: implemented
Checked by: a_forked_childs_put_goes_to_the_side_file, a_busy_slot_at_take_fails_the_fold_even_with_side_records, golden:gather-native, golden:fold-native, tla:ForkSide_fold, tla:ForkSide_fold_replay_skipped.bug, tla:ForkSide_fold_replay_over_busy.bug

After the seal, take closes the side stream if the entry is SIDE (SIDE-7)
and the bracket folds each committed record through `step` into one more
value, made from a fresh init, which it combines with the slots. The seal
keeps every other put out of the slot table, so the replay needs no lock
while it runs `step`. A take that finds a slot not FREE fails before any
replay (FOLD-4), so no replay can clear a BUSY mark and hide the batches
a thrown step lost. The replay value is combined after the slots.

### FOLD-4 A BUSY or FAILED slot at take fails the fold
Status: implemented
Checked by: a_busy_slot_at_take_fails_the_fold_even_with_side_records, a_failed_step_fails_later_puts_and_the_fold, a_folds_errors_name_the_fold, golden:fold-step-failures, tla:ForkSide_fold, tla:ForkSide_fold_reduce_ignores_busy.bug, tla:ForkSide_fold_replay_over_busy.bug

Before it combines, take checks every slot; if any is not FREE the fold fails
loudly and nothing is combined. A BUSY slot holds no value: its batches,
which returned success, are in a thread's hands (a producer thread still
stepping, which escaped the producer callback) or gone (a step that
threw). Combining the moved-from value would drop them silently.

A step that throws fails its put and marks the slot FAILED, so the fold
fails at take even when the producer catches the put's error. So does a
fresh slot's copy of init that throws, and a step that unwinds by any
other means (an R condition that is not an error, an interrupt). Every
later put, on any thread, fails; its message says that an earlier step
failed, not that the put is re-entrant. A step that ends its
thread without marking the slot leaves it BUSY, which fails the take the
same way. A fold's errors name the fold, never a gather.

### FOLD-5 A re-entrant put fails and changes nothing
Status: implemented
Checked by: a_reentrant_put_fails_and_changes_nothing, tla:ForkSide_fold, tla:ForkSide_fold_reentry_no_busy.bug

A put from a thread that holds a BUSY slot (the step called the sink, or
a handle the user stashed) fails with "re-entrant fold put" and leaves the
slot and its checked-out value as they were. The third phase finds a
put's slot by the thread that holds it, so a thread holds at most one.
Without the check, an inner put that steps the moved-from value in the
outer put's slot is overwritten by the outer put's store: the inner put
returned success and its batch is lost.

### FOLD-6 Each slot starts from its own copy of init
Status: implemented
Checked by: a_put_while_another_steps_folds_into_its_own_fresh_slot, golden:fold-native, golden:fold-r-reference, golden:spec-strm-5-1, tla:ForkSide_fold, tla:ForkSide_fold_shared_init.bug

A slot, the replay value and an empty fold's result start from a fresh
init: Python `copy.deepcopy(init)`, Rust `B: Clone`, C++ the copy
constructor, R `unserialize(serialize(init, NULL))` (a deep copy, since an
environment or R6 init is passed by reference). Since a step may mutate its accumulator
(FOLD-1), slots sharing one init object would fold every batch into all of
them, and the combine would count each several times; a sourced init is a
module-level object, so later folds would start from a dirty one. The
first phase makes a new slot's value under the entry lock with no user
code and, in Python, no allocation: C++, Python and Rust only take a
reference to init (a counted C++ reference, a Python reference, a Rust
`Arc`), which the put copies and drops after unlocking, in its second
phase, so a copy that throws fails the put as a step does (FOLD-4); R
takes init itself, which the bracket's frame holds, and copies it the
same way. So a copy never outlives the init it was made from, even when
the bracket returns while a put is still in its first phase.

### FOLD-7 The fold laws
Status: implemented
Checked by: golden:spec-strm-5-1

The laws are the language's STRM-5 (ruled 2026-11-14). The homomorphism
over splits is not implied by the other three: init 0, step b c = b + 1,
combine = max satisfies them yet depends on the split.

### FOLD-8 A Rust accumulator is Send
Status: implemented
Checked by: producer_threads_never_copy_init_at_once, golden:fold-native, golden:spec-native-9-10

The take thread combines values built on producer threads, so a Rust
accumulator type must be `Send`. It need not be `Sync`: with the checkout
protocol each slot's value has one user at a time, and the fold's one
init, which every producer thread copies (FOLD-6), is held in a mutex of
its own, so no two threads copy it at once. It is also `Clone` (FOLD-6).
A gather's batch type is `Send` for the same reason.

### FOLD-9 A Python or R fold fed from another pool takes the file route
Status: implemented
Checked by: golden:fold-native, golden:fold-placement

When the fold's home is a fork pool (Python, R) and its handle crosses
(handleCrosses), sibling workers are not forked from the bracket's worker
and have no entry (NET-9). The put then
writes and flushes each batch to a temp-file stream, so each is its own
frame, and the bracket replays the stream through step on its own thread
when the producer returns. A batch type with no wire form there is a
compile-time error. The fold holds its batches on disk until then and answers only
after. A C++ or Rust put given a stream handle writes and flushes the
batch the same way, so a put placed outside the fold's pool is never
wrong. A C++ or Rust home is one process, so its puts always check out
natively. A producer in a remote job (a SLURM label) is in neither the
home's process nor its file's: no route serves it (issues/remote-dispatch.md).

### FOLD-10 A put still stepping when take returns touches nothing take freed
Status: implemented
Checked by: a_store_after_the_seal_fails_and_the_fold_fails, golden:fold-native

A producer thread that escaped the callback may be in its unlocked step
when take fails the fold (FOLD-4) and the bracket returns. Its third phase
finds the entry by id and fails late if the entry is gone (FOLD-1), and
the put drops its value. The step is the put's own argument, held by the
sink closure that called it, and a fresh value never borrows the bracket's
init (FOLD-6), so the put uses nothing the bracket frees.

### FOLD-11 A sequential producer's batches fold in the order it put them
Status: implemented
Checked by: sequential_puts_on_alternating_threads_keep_their_order, puts_from_more_threads_than_the_slot_cap_share_one_slot, golden:fold-sequential-order, tla:ForkSide_fold, tla:ForkSide_fold_checkout_by_thread.bug

When no two of a producer's puts are in flight at once and none comes
from a forked child, the fold is `foldl step init xs` over the batches in
the order they were put, with no need for commutativity (STRM-5). Such a
producer's puts may still be served by different pool threads: a callback
from another pool runs on whichever worker serves its connection, and a
producer whose threads take turns, or that reconnects (NET-7, FORK-14,
NET-9), or whose workers retire, reaches several. Since a put takes the
lowest FREE slot, and with no put in flight every slot is FREE, every
batch of a sequential producer folds into the first slot, in put order.
Slots keyed by thread instead would split the batches across slots that
take combines in slot order, out of the order they were put, and would
grow with every thread that ever put until MAX_SLOTS fails the fold. The
slot count is bounded by the puts in flight at once.
