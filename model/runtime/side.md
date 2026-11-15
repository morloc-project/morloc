# Fork side

An in-pool accumulator (a native gather's list, a fold's slot table) lives in one
process's memory. A forked child that calls the accumulator's sink would
append to its own copy-on-write copy, and the batch would be lost. The fork
side is the runtime record that sends those batches through a side file
instead (SIDE-10). Each entry has an id, an owner fork generation, the
call that made it (whose temp files it joins), and one atomic state word:
NATIVE, SWITCHING, SIDE (with its file), FAILED or TAKEN. Every
change of the word is a compare-and-swap. The registry is process-local and
its lock is a held lock (FORK-5), so a child sees its parent's entries.

### SIDE-1 The state word moves only forward
Status: implemented
Checked by: a_put_after_take_fails_and_take_without_fork_has_no_side, a_fork_between_takes_read_and_its_end_loses_no_child_batch, tla:ForkSide, tla:ForkSide_take_skips_cas.bug

An entry starts NATIVE. The fork prepare handler moves NATIVE to
SWITCHING and then to SIDE once the side stream is open, or to FAILED if
the open failed. Take, and dropping an unread entry, move NATIVE, SIDE or
FAILED to TAKEN, after waiting out SWITCHING; prepare leaves TAKEN alone.
No other move exists. Take and prepare race on the same CAS,
so exactly one of them wins: a take that wins leaves nothing for a child to
write into, and a prepare that wins makes the take close and read the side.
A take that loads the word and stores TAKEN later lets a prepare open a
side stream nobody reads, and a child's returned write is lost.

### SIDE-2 A child never inherits an unsettled entry its parent owns
Status: implemented
Checked by: tla:ForkSide, tla:ForkSide_two, tla:ForkSide_born_in_window.bug, tla:ForkSide_child_sees_switching.bug, tla:ForkSide_open_failure_left_native.bug

At the fork, every entry the forking process owns is SIDE, FAILED or TAKEN.
The prepare handler raises the process's forking count, under the registry
lock, before it reads the registry, and an entry is made only under that
lock with the count at zero; the count drops in the parent's after-fork
handler. So no entry is born between prepare's snapshot and the fork. An
entry maker waits for the count with the registry lock released. A prepare
that finds an entry SWITCHING, because another thread's prepare is opening
its side, waits until it is SIDE or FAILED before it forks. A failed open
stores FAILED, never NATIVE.

### SIDE-3 A forked child starts with no fork in progress
Status: implemented
Checked by: tla:ForkSide_two, tla:ForkSide_child_decrements_count.bug

The child's forking count is set to zero in its after-fork handler, not
lowered: the count it inherits includes prepares by other parent threads,
which have no thread in the child to finish them. Lowering it leaves an
entry maker in the child waiting forever.

### SIDE-4 A put goes where its process and the state word say
Status: implemented
Checked by: an_unordered_owner_puts_natively_until_take, a_fold_owners_side_write_is_refused_and_writes_nothing, a_call_of_the_other_kind_is_refused_and_leaves_the_entry, a_forked_childs_put_goes_to_the_side_file, a_gather_owner_writes_the_side_once_switched, a_schemaless_child_batch_fails_the_take, golden:gather-native, golden:gather-crossing, golden:gather-relay, golden:gather-fold-packed, golden:fold-native, tla:ForkSide, tla:ForkSide_fold, tla:ForkSide_route_by_pid.bug, tla:ForkSide_fold_route_by_pid.bug

Ownership is the entry's fork generation (FORK-14), never the pid. In the
owner:
- a gather appends to the native list while the word is NATIVE or
  SWITCHING, and writes to the side stream while it is SIDE;
- a fold checks its slot out natively in every state but TAKEN (FOLD-1);
- a gather put on FAILED fails;
- a side write of a put that routes natively (any fold put, a gather put
  while the word is not SIDE) is refused and writes nothing.
A call made for one kind of entry (gather or fold) on an entry of the
other kind fails and leaves the entry as it was; it never reads the other
kind's native pointer. A drop of the wrong kind is fatal.
A fold put whose id has no entry in its process fails as a gather put does
(SIDE-11); a Python or R fold fed from another pool takes the file route
instead (FOLD-9).
A gather put chooses its route by the batch's type alone: where the type
can be held natively, one put dispatches on the handle at run time, a
gather id (negative) to this record, a stream handle to the stream.
In any other process a put writes to the side stream if the word is SIDE
and fails otherwise; NATIVE or SWITCHING there is an internal error (a
handle that crossed a process without the fork handler). A put on TAKEN
fails. A child never touches the native list or its mutex, which it may
have inherited locked; it forgets them and never destroys them.

### SIDE-5 A put that returned is in the result exactly once
Status: implemented
Checked by: a_write_after_take_fails, a_store_after_the_seal_fails_and_the_fold_fails, a_truncated_record_fails_the_take, an_owner_write_racing_take_is_in_the_result_or_fails, owner_threads_writing_while_take_runs_lose_and_tear_nothing, tla:ForkSide, tla:ForkSide_two, tla:ForkSide_nested, tla:ForkSide_fold, tla:ForkSide_append_ignores_taken.bug, tla:ForkSide_fold_store_ignores_seal.bug, tla:ForkSide_take_skips_cas.bug

A native append checks, under the list's mutex, that take has not yet
drained the list, and fails if it has; take drains under the same mutex. A
side write that finds the stream closed fails (SLOT-11, SLOT-13 hold for
the writes that got in). Dropping an entry clears its list under the same
mutex, so its owner may free the list once the drop returns. So a put
that read the word before take's CAS
either lands before the drain or close and is in the result, or fails. No
put returns success after take has returned. For a fold the drain is the
seal (FOLD-2): a store after it fails late (FOLD-1), and the side replay
(FOLD-3) folds the child batches in.

### SIDE-6 A gather's result keeps causal order
Status: implemented
Checked by: golden:gather-native, tla:ForkSide, tla:ForkSide_two, tla:ForkSide_nested, tla:ForkSide_owner_stays_native.bug

The result is the native list, then the side stream's elements in slot-lock
order. A put that began after another returned, in the same thread, in a
child forked after it, or in a thread that reaped the child that made it,
comes later in the result. Every native append's route read precedes the
fork that switched the entry, and every child write follows it; a parent
put that read NATIVE or SWITCHING before the switch and appends after it is
concurrent with every child write. A fold's result has no order (its
laws are FOLD-7).

### SIDE-7 Take in the owner closes the side; take anywhere else closes nothing
Status: implemented
Checked by: a_forked_child_cannot_take_and_its_record_reaches_the_owner, tla:ForkSide, tla:ForkSide_take_in_child_closes.bug, tla:ForkSide_route_by_pid.bug

Take in the owner CASes NATIVE or SIDE to TAKEN, waiting while the word is
SWITCHING. From SIDE it closes the side stream and returns it to be
decoded after the native list (gather) or replayed through step (fold,
FOLD-3). Take in any other process fails and leaves the
stream open: the owner's take is the only close, and a child's would cut
off the parent's and its siblings' later writes.

### SIDE-8 A failed side open fails the take
Status: implemented
Checked by: a_gather_owner_writes_the_side_once_switched, tla:ForkSide, tla:ForkSide_take_ignores_failed.bug, tla:ForkSide_open_failure_left_native.bug

Take on FAILED fails, in the parent, so a child batch that could not be
written is never lost silently: the child's put fails too, but only the
parent's failure is sure to reach the caller.

### SIDE-9 Only a fork that runs the fork handlers switches an entry
Status: implemented
Checked by: a_fork_settles_a_live_native_entry_and_a_spawn_does_not

Every fork that runs the atfork handlers while an entry is live switches
it, including a fork unrelated to the gather (another call in a daemon, a
library forking a helper): correct, and that gather takes the file route
from then on. posix_spawn and vfork run no handlers and switch nothing. A
child made without the handlers that goes on running the runtime is outside
this record, as in FORK-14. The pool runtime's own worker forks never run
while an entry is live (FORK-6 gate).

### SIDE-10 A side file holds whole records in the order their writers took its lock
Status: implemented
Checked by: a_forked_child_cannot_take_and_its_record_reaches_the_owner, a_write_after_take_fails, a_truncated_record_fails_the_take, a_schemaless_child_batch_fails_the_take, owner_threads_writing_while_take_runs_lose_and_tear_nothing, a_write_that_fails_midway_leaves_no_torn_record, a_failed_commit_leaves_exactly_the_committed_records, a_torn_commit_fails_later_writes_and_the_take

A side file is a temp file of the entry's call: a header of a state byte,
the committed length and that length's complement, then records of a
length and a batch in flat form. Every write, in any thread or process,
opens the file itself and holds an exclusive flock on that open file
description (a flock excludes other descriptions only, so threads sharing
one would not exclude each other) while it reads the header, writes its
record at the committed length and then writes the new committed length:
a closed file refuses the write. The put succeeds only if both writes
did. Bytes past the committed length are never read, and the next write
overwrites them, so a write that fails partway, or a writer that dies
inside the lock, leaves nothing a reader sees; its put returned an error
or never returned. A failed write of the committed length is a failed
put: if it changed nothing, its record stays uncommitted; if it tore the
length, the length no longer matches its complement, and every later
write and the take fail. The owner's take marks the file closed under
the same kind of lock, notes the committed length, releases the lock and
removes the file; a write that opens after the removal fails. No record
is written once the file is closed, so the take then reads the committed
records one at a time with pread on that same descriptor (a second open
could drop an emulated lock on NFS), never the whole file at once.
Dropping an unread entry closes and removes its file without reading it.
A returned write is in the file whatever its writer does next. A child
that cannot encode its batch (a fold whose batch type has no wire form in
its language) writes a poison record in its place, and the take fails.

### SIDE-11 A gather or fold id names one process's record in the whole program
Status: implemented
Checked by: gather_ids_are_program_unique_and_a_foreign_one_is_refused, golden:fold-native

Each process draws an ordinal from a counter in the program's primary
volume the first time it makes a gather or a fold, and again after a
fork. An id packs the ordinal with a number counted in the process. So no
two processes of a program make the same id, and an id that reaches
another process names nothing there. A gather or fold put whose id has no entry
in its process fails: as a late put if the process or an ancestor made the
id, and otherwise as a handle that reached another process. It never
appends to another gather.
