----------------------------- MODULE StdioBracket -----------------------------
(* A @collect bracket streaming to stdout (model/runtime/streams.md SLOT-16, *)
(* model/runtime/fork.md FORK-17). The parent opens the stream, calls the   *)
(* producer, which puts batches and may fork, and closes the stream once   *)
(* the call returns. A forked child shares the stream: it may put, leave   *)
(* by _exit, die (also while it holds the slot lock), or return from the   *)
(* producer call into the bracket. A sourced call records the fork         *)
(* generation on entry; a process whose generation differs when the call   *)
(* returns exits there. Only the stream's opener may close it.             *)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANT Variant
\* "design": child writes accepted, the per-call check, the close pinned.
\* "write_pinned": a child's write is refused (the stdio pin before the fix).
\* "no_call_check": the check runs only at the end of the dispatch, so a
\*   child that returns from the producer runs the bracket's close; the
\*   close pin refuses it.
\* "child_closes": no per-call check and no close pin.

Procs == {"P", "C"}
ParentPuts == {"p1", "p2"}
ChildPuts == {"c1"}

WritePinned == Variant = "write_pinned"
CallCheck == Variant \in {"design", "write_pinned"}
ClosePinned == Variant # "child_closes"

VARIABLES
    pcP,       \* "open" "inside" "after" "close" "done"
    pcC,       \* "none" "inside" "after" "close" "exited" "dead" "done"
    gen,       \* fork generation of each process
    entry,     \* generation each process recorded entering the producer call
    out,       \* the stream's batches, in the order they were committed
    returned,  \* puts that returned success
    attempted, \* puts begun
    lock,      \* "" or the process holding the slot lock mid-put
    pending,   \* the batch each process is committing
    closed, closer, failed,
    refusedLive \* a live child's put to an open, unfailed stream was refused

vars == <<pcP, pcC, gen, entry, out, returned, attempted, lock, pending,
          closed, closer, failed, refusedLive>>

Range(s) == {s[i] : i \in 1..Len(s)}

Init ==
    /\ pcP = "open" /\ pcC = "none"
    /\ gen = [p \in Procs |-> 0] /\ entry = [p \in Procs |-> 0]
    /\ out = <<>> /\ returned = {} /\ attempted = {}
    /\ lock = "" /\ pending = [p \in Procs |-> ""]
    /\ closed = FALSE /\ closer = "" /\ failed = FALSE /\ refusedLive = FALSE

Inside(q) == IF q = "P" THEN pcP = "inside" ELSE pcC = "inside"

\* Open: the parent opens stdout and enters the producer call.
Open ==
    /\ pcP = "open"
    /\ pcP' = "inside"
    /\ entry' = [entry EXCEPT !["P"] = gen["P"]]
    /\ UNCHANGED <<pcC, gen, out, returned, attempted, lock, pending, closed, closer, failed, refusedLive>>

\* A put takes the slot lock, then commits its batch. A put to a closed
\* stream is late: it raises and changes nothing (STRM-9). A refused put to
\* an open stream raises and marks the stream failed (STRM-9, SLOT-35).
PutBegin(q, b) ==
    /\ Inside(q) /\ b \notin attempted /\ lock = "" /\ pending[q] = ""
    /\ attempted' = attempted \union {b}
    /\ IF closed \/ failed
         THEN UNCHANGED <<lock, pending, failed, refusedLive>>
       ELSE IF q = "C" /\ WritePinned
         THEN /\ failed' = TRUE
              /\ refusedLive' = TRUE
              /\ UNCHANGED <<lock, pending>>
       ELSE /\ lock' = q
            /\ pending' = [pending EXCEPT ![q] = b]
            /\ UNCHANGED <<failed, refusedLive>>
    /\ UNCHANGED <<pcP, pcC, gen, entry, out, returned, closed, closer>>

PutEnd(q) ==
    /\ lock = q /\ pending[q] # ""
    /\ out' = Append(out, pending[q])
    /\ returned' = returned \union {pending[q]}
    /\ lock' = ""
    /\ pending' = [pending EXCEPT ![q] = ""]
    /\ UNCHANGED <<pcP, pcC, gen, entry, attempted, closed, closer, failed, refusedLive>>

\* Fork copies the parent's record of the call; the child's generation
\* differs (FORK-14).
Fork ==
    /\ pcP = "inside" /\ pcC = "none" /\ lock = ""
    /\ pcC' = "inside"
    /\ gen' = [gen EXCEPT !["C"] = gen["P"] + 1]
    /\ entry' = [entry EXCEPT !["C"] = entry["P"]]
    /\ UNCHANGED <<pcP, out, returned, attempted, lock, pending, closed, closer, failed, refusedLive>>

Return(q) ==
    /\ Inside(q) /\ pending[q] = ""
    /\ IF q = "P" THEN pcP' = "after" /\ UNCHANGED pcC ELSE pcC' = "after" /\ UNCHANGED pcP
    /\ UNCHANGED <<gen, entry, out, returned, attempted, lock, pending, closed, closer, failed, refusedLive>>

\* FORK-17: the check when the producer call returns.
After(q) ==
    /\ IF q = "P" THEN pcP = "after" ELSE pcC = "after"
    /\ LET leave == CallCheck /\ gen[q] # entry[q]
           next == IF leave THEN "exited" ELSE "close"
       IN IF q = "P" THEN pcP' = next /\ UNCHANGED pcC ELSE pcC' = next /\ UNCHANGED pcP
    /\ UNCHANGED <<gen, entry, out, returned, attempted, lock, pending, closed, closer, failed, refusedLive>>

\* The bracket's close waits out a put in progress. A close from a process
\* other than the opener is refused when the close is pinned; that process
\* raises and exits at the end of its dispatch.
Close(q) ==
    /\ IF q = "P" THEN pcP = "close" ELSE pcC = "close"
    /\ lock = ""
    /\ IF q # "P" /\ ClosePinned
         THEN UNCHANGED <<closed, closer>>
         ELSE /\ closed' = TRUE
              /\ closer' = IF closer = "" THEN q ELSE closer
    /\ IF q = "P" THEN pcP' = "done" /\ UNCHANGED pcC
                  ELSE pcC' = "exited" /\ UNCHANGED pcP
    /\ UNCHANGED <<gen, entry, out, returned, attempted, lock, pending, failed, refusedLive>>

ChildExit ==
    /\ pcC = "inside" /\ pending["C"] = ""
    /\ pcC' = "exited"
    /\ UNCHANGED <<pcP, gen, entry, out, returned, attempted, lock, pending, closed, closer, failed, refusedLive>>

\* SLOT-14: a writer that dies holding the slot lock fails the stream.
ChildDies ==
    /\ pcC \in {"inside", "after", "close"}
    /\ pcC' = "dead"
    /\ IF lock = "C"
         THEN /\ failed' = TRUE /\ lock' = ""
              /\ pending' = [pending EXCEPT !["C"] = ""]
         ELSE UNCHANGED <<failed, lock, pending>>
    /\ UNCHANGED <<pcP, gen, entry, out, returned, attempted, closed, closer, refusedLive>>

Parent ==
    \/ Open \/ Fork \/ PutEnd("P") \/ Return("P") \/ After("P") \/ Close("P")
    \/ \E b \in ParentPuts : PutBegin("P", b)
Child ==
    \/ PutEnd("C") \/ Return("C") \/ After("C") \/ Close("C") \/ ChildExit \/ ChildDies
    \/ \E b \in ChildPuts : PutBegin("C", b)

Done ==
    /\ pcP = "done" /\ pcC \in {"none", "exited", "dead"}
    /\ UNCHANGED vars

Next == Parent \/ Child \/ Done

Spec == Init /\ [][Next]_vars /\ WF_vars(Parent) /\ WF_vars(Child)

\* STRM-8: the bracket that opened the stream is its only closer.
OnlyOpenerCloses == closer \in {"", "P"}
\* FORK-17: a child that returns from the call made before its fork runs no
\* more of the bracket.
ChildNeverRunsBracketCode == pcC # "close"
\* SLOT-16: any process holding the handle may write it.
NoLivePutRefused == ~refusedLive
\* A put that returned is in the output.
ReturnedPutsAreInTheOutput == returned \subseteq Range(out)
\* Every stream ends, closed or failed.
TheStreamEnds == <>(closed \/ failed)
=============================================================================
