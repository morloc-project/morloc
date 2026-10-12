---------------------------- MODULE ArenaCheckpoint ----------------------------
(* An eval arena's checkpoint (model/runtime/shm.md SHM-7). The arena records   *)
(* a block each time the eval allocates; freeing a block drops its latest     *)
(* entry with a constant-time swap-remove. A failed `@catch` body rolls back  *)
(* to the checkpoint taken before it: the entries recorded since are released *)
(* and those recorded before are kept. One thread.                             *)
EXTENDS Naturals, Sequences

CONSTANT Variant, Addrs, MaxRecords
\* "design": a checkpoint is the number of entries ever recorded, and an
\*   entry carries the number recorded before it.
\* "length_checkpoint": a checkpoint is the arena's length.

VARIABLES
    log,       \* the arena's entries, in storage order: [addr, seq]
    recorded,  \* entries ever recorded
    cp,        \* the checkpoint, or none before it is taken
    cpLen,     \* the arena's length when the checkpoint was taken
    phase,     \* "before" | "after" (checkpoint taken) | "rolled"
    lost,      \* an entry recorded after the checkpoint survived the rollback
    early      \* an entry recorded before the checkpoint was released by it

vars == <<log, recorded, cp, cpLen, phase, lost, early>>

Init ==
    /\ log = <<>> /\ recorded = 0 /\ cp = 0 /\ cpLen = 0
    /\ phase = "before" /\ lost = FALSE /\ early = FALSE

Record(a) ==
    /\ phase # "rolled" /\ recorded < MaxRecords
    /\ log' = Append(log, [addr |-> a, seq |-> recorded])
    /\ recorded' = recorded + 1
    /\ UNCHANGED <<cp, cpLen, phase, lost, early>>

\* The last index of an entry for a, which a free removes.
Last(a) == CHOOSE i \in 1..Len(log) :
              log[i].addr = a /\ \A j \in (i+1)..Len(log) : log[j].addr # a

\* swap_remove: the last entry fills the hole.
SwapRemove(i) ==
    IF i = Len(log) THEN SubSeq(log, 1, Len(log) - 1)
    ELSE [SubSeq(log, 1, Len(log) - 1) EXCEPT ![i] = log[Len(log)]]

Free(a) ==
    /\ phase # "rolled"
    /\ \E i \in 1..Len(log) : log[i].addr = a
    /\ log' = SwapRemove(Last(a))
    /\ UNCHANGED <<recorded, cp, cpLen, phase, lost, early>>

Checkpoint ==
    /\ phase = "before"
    /\ phase' = "after" /\ cp' = recorded /\ cpLen' = Len(log)
    /\ UNCHANGED <<log, recorded, lost, early>>

Released(i) == IF Variant = "length_checkpoint" THEN i > cpLen ELSE log[i].seq >= cp

Rollback ==
    /\ phase = "after"
    /\ LET keep == SelectSeq(log, LAMBDA e : \E i \in 1..Len(log) : log[i] = e /\ ~Released(i))
       IN /\ log' = keep
          /\ lost' = \E i \in 1..Len(keep) : keep[i].seq >= cp
          /\ early' = \E i \in 1..Len(log) : Released(i) /\ log[i].seq < cp
    /\ phase' = "rolled"
    /\ UNCHANGED <<recorded, cp, cpLen>>

Next == \/ \E a \in Addrs : Record(a) \/ Free(a)
        \/ Checkpoint \/ Rollback
        \/ (phase = "rolled" /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars

\* ---- properties ----
RollbackReleasesWhatFollows == ~lost
RollbackKeepsWhatPrecedes == ~early
================================================================================
