------------------------------- MODULE ArenaRefs -------------------------------
(* The references an eval arena stands for (model/runtime/shm.md SHM-7,      *)
(* SHM-14). Every reference has an identity and one holder kind:             *)
(*   "arena"  only the arena releases it (an eval value);                    *)
(*   "own"    a handle frees it with shfree, which also drops the arena      *)
(*            entry for its block (code freeing what it allocated, a         *)
(*            sub-packet read whose block the eval allocated or was given);  *)
(*   "acq"    a handle that took its own reference on a live block (a cache *)
(*            hit, an adopted reply) and releases it without the arena;      *)
(*   "struct" a runtime structure (stream slot, cell, Arrow view, cache)     *)
(*            that releases it without the arena.                            *)
(* The arena knows only addresses: its log holds [addr, seq] entries in the  *)
(* order they were recorded, and a free names a block, not a reference. An   *)
(* allocation records its block; a structure's reference may be given to    *)
(* the arena when the structure held the block alone (a decoded-ahead block  *)
(* taken from the cache). A rollback to a checkpoint releases the entries    *)
(* recorded after it; the arena's end releases the rest. Handles made        *)
(* inside the failed body are gone before the rollback. One thread.          *)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANT Variant, Addrs, MaxRefs, MaxRecords
\* "design": the design.
\* "hit_recorded": a cache hit records its reference in the arena and frees
\*   it with shfree, as an allocation does.
\* "hit_recorded_swap": the same, with a free filling its place with the
\*   log's last entry.
\* "acquired_shfree": a handle frees a reference it took for itself with
\*   shfree, dropping an entry it never had.
\* "struct_forgets": a structure's release also drops an arena entry.
\* "handoff_keeps_entry": a hand-off to a structure leaves the arena's entry.
\* "length_checkpoint": a checkpoint is the log's length, and a rollback
\*   releases the entries placed after it.

VARIABLES
    refs,      \* every reference made: [addr, kind, born, live]
    log,       \* the arena's entries in storage order: [addr, seq]
    seq,       \* entries ever recorded
    cp,        \* the checkpoint, as a seq
    cpLen,     \* the log's length when the checkpoint was taken
    phase,     \* "before" | "body" | "after" (rolled back) | "ended"
    early,     \* the arena released a reference someone else holds
    missed,    \* a rollback kept an entry recorded after its checkpoint
    overshot,  \* a rollback released an entry recorded before it
    underflow  \* a release found no reference left on its block

vars == <<refs, log, seq, cp, cpLen, phase, early, missed, overshot, underflow>>

Kinds == {"arena", "own", "acq", "struct"}

Init ==
    /\ refs = <<>> /\ log = <<>> /\ seq = 0 /\ cp = 0 /\ cpLen = 0
    /\ phase = "before" /\ early = FALSE /\ underflow = FALSE
    /\ missed = FALSE /\ overshot = FALSE

Ids == 1..Len(refs)
Live(a) == {r \in Ids : refs[r].live /\ refs[r].addr = a}
Count(a) == Cardinality(Live(a))
Entries(a) == {i \in 1..Len(log) : log[i].addr = a}
Born == IF phase = "body" THEN "post" ELSE "pre"
Open == phase # "ended"
Swaps == Variant = "hit_recorded_swap"
HitsRecord == Variant \in {"hit_recorded", "hit_recorded_swap"}

New(a, k) == Append(refs, [addr |-> a, kind |-> k, born |-> Born, live |-> TRUE])
Room == seq < MaxRecords
Record(a) == Append(log, [addr |-> a, seq |-> seq])
End(r) == [refs EXCEPT ![r].live = FALSE]
Becomes(r, k) == [refs EXCEPT ![r].kind = k]

Drop(l, i) ==
    IF Swaps /\ i < Len(l)
    THEN [SubSeq(l, 1, Len(l) - 1) EXCEPT ![i] = l[Len(l)]]
    ELSE SubSeq(l, 1, i - 1) \o SubSeq(l, i + 1, Len(l))

\* Drop the last-placed entry for a, if there is one.
Forget(l, a) ==
    IF \E i \in 1..Len(l) : l[i].addr = a
    THEN Drop(l, CHOOSE i \in 1..Len(l) :
                    l[i].addr = a /\ \A j \in (i+1)..Len(l) : l[j].addr # a)
    ELSE l

\* A fresh block: no live reference has its address.
Alloc(a, k) ==
    /\ Open /\ Len(refs) < MaxRefs /\ Room /\ k \in {"arena", "own"}
    /\ Count(a) = 0
    /\ refs' = New(a, k) /\ log' = Record(a) /\ seq' = seq + 1
    /\ UNCHANGED <<cp, cpLen, phase, early, missed, overshot, underflow>>

\* A handle takes another reference on a live block (a cache hit).
Acquire(a) ==
    /\ Open /\ Len(refs) < MaxRefs /\ Room /\ Count(a) > 0
    /\ IF HitsRecord
       THEN /\ refs' = New(a, "own") /\ log' = Record(a) /\ seq' = seq + 1
       ELSE /\ refs' = New(a, "acq") /\ UNCHANGED <<log, seq>>
    /\ UNCHANGED <<cp, cpLen, phase, early, missed, overshot, underflow>>

\* A structure takes a reference of its own on a live block (a cache
\* installing a sub-packet the eval just decoded).
StructAcquire(a) ==
    /\ Open /\ Len(refs) < MaxRefs /\ Count(a) > 0
    /\ refs' = New(a, "struct")
    /\ UNCHANGED <<log, seq, cp, cpLen, phase, early, missed, overshot, underflow>>

OwnFree(r) ==
    /\ Open /\ r \in Ids /\ refs[r].live /\ refs[r].kind = "own"
    /\ refs' = End(r) /\ log' = Forget(log, refs[r].addr)
    /\ UNCHANGED <<seq, cp, cpLen, phase, early, missed, overshot, underflow>>

AcqRelease(r) ==
    /\ Open /\ r \in Ids /\ refs[r].live /\ refs[r].kind = "acq"
    /\ refs' = End(r)
    /\ log' = IF Variant = "acquired_shfree" THEN Forget(log, refs[r].addr) ELSE log
    /\ UNCHANGED <<seq, cp, cpLen, phase, early, missed, overshot, underflow>>

\* A handle leaves its block in the value it built (into_block).
Escape(r) ==
    /\ Open /\ r \in Ids /\ refs[r].live /\ refs[r].kind = "own"
    /\ refs' = Becomes(r, "arena")
    /\ UNCHANGED <<log, seq, cp, cpLen, phase, early, missed, overshot, underflow>>

\* A recorded reference is handed to a structure.
HandOff(r) ==
    /\ Open /\ r \in Ids /\ refs[r].live /\ refs[r].kind \in {"arena", "own"}
    /\ refs' = Becomes(r, "struct")
    /\ log' = IF Variant = "handoff_keeps_entry" THEN log ELSE Forget(log, refs[r].addr)
    /\ UNCHANGED <<seq, cp, cpLen, phase, early, missed, overshot, underflow>>

\* A structure that held a block alone gives its reference to the eval.
Adopt(r) ==
    /\ Open /\ Room /\ r \in Ids /\ refs[r].live
    /\ refs[r].kind = "struct" /\ Count(refs[r].addr) = 1
    /\ refs' = [refs EXCEPT ![r].kind = "own", ![r].born = Born]
    /\ log' = Record(refs[r].addr) /\ seq' = seq + 1
    /\ UNCHANGED <<cp, cpLen, phase, early, missed, overshot, underflow>>

StructRelease(r) ==
    /\ r \in Ids /\ refs[r].live /\ refs[r].kind = "struct"
    /\ refs' = End(r)
    /\ log' = IF Variant = "struct_forgets" THEN Forget(log, refs[r].addr) ELSE log
    /\ UNCHANGED <<seq, cp, cpLen, phase, early, missed, overshot, underflow>>

\* The arena releases one reference on a: one only it holds, if any.
ArenaRelease(rs, a) ==
    LET mine == {r \in 1..Len(rs) : rs[r].live /\ rs[r].addr = a /\ rs[r].kind = "arena"}
        any == {r \in 1..Len(rs) : rs[r].live /\ rs[r].addr = a}
    IN IF mine # {} THEN [rs EXCEPT ![CHOOSE r \in mine : TRUE].live = FALSE]
       ELSE IF any # {} THEN [rs EXCEPT ![CHOOSE r \in any : TRUE].live = FALSE]
       ELSE rs

Stolen(rs, a) ==
    /\ \A r \in 1..Len(rs) : ~(rs[r].live /\ rs[r].addr = a /\ rs[r].kind = "arena")
    /\ \E r \in 1..Len(rs) : rs[r].live /\ rs[r].addr = a

Gone(rs, a) == \A r \in 1..Len(rs) : ~(rs[r].live /\ rs[r].addr = a)

RECURSIVE Drain(_, _, _, _, _)
Drain(d, i, rs, e, u) ==
    IF i > Len(d) THEN [refs |-> rs, early |-> e, underflow |-> u]
    ELSE Drain(d, i + 1, ArenaRelease(rs, d[i].addr), e \/ Stolen(rs, d[i].addr), u \/ Gone(rs, d[i].addr))

Checkpoint ==
    /\ phase = "before"
    /\ phase' = "body" /\ cp' = seq /\ cpLen' = Len(log)
    /\ UNCHANGED <<refs, log, seq, early, missed, overshot, underflow>>

Later(i) == IF Variant = "length_checkpoint" THEN i > cpLen ELSE log[i].seq >= cp

\* The body failed: its handles are gone.
Rollback ==
    /\ phase = "body"
    /\ \A r \in Ids : ~(refs[r].live /\ refs[r].kind \in {"own", "acq"} /\ refs[r].born = "post")
    /\ LET ds == SelectSeq(log, LAMBDA e : \E j \in 1..Len(log) : log[j] = e /\ Later(j))
           k == SelectSeq(log, LAMBDA e : \E j \in 1..Len(log) : log[j] = e /\ ~Later(j))
           res == Drain(ds, 1, refs, early, underflow)
       IN /\ refs' = res.refs /\ log' = k
          /\ early' = res.early /\ underflow' = res.underflow
          /\ missed' = \E i \in 1..Len(k) : k[i].seq >= cp
          /\ overshot' = \E i \in 1..Len(ds) : ds[i].seq < cp
    /\ phase' = "after"
    /\ UNCHANGED <<seq, cp, cpLen>>

\* The scope ends once no handle is left.
EndScope ==
    /\ Open
    /\ \A r \in Ids : ~(refs[r].live /\ refs[r].kind \in {"own", "acq"})
    /\ LET res == Drain(log, 1, refs, early, underflow)
       IN /\ refs' = res.refs /\ early' = res.early /\ underflow' = res.underflow
    /\ log' = <<>> /\ phase' = "ended"
    /\ UNCHANGED <<seq, cp, cpLen, missed, overshot>>

Done == phase = "ended" /\ \A r \in Ids : ~(refs[r].live /\ refs[r].kind = "struct")

Next ==
    \/ \E a \in Addrs, k \in {"arena", "own"} : Alloc(a, k)
    \/ \E a \in Addrs : Acquire(a) \/ StructAcquire(a)
    \/ \E r \in 1..MaxRefs : OwnFree(r) \/ AcqRelease(r) \/ Escape(r) \/ HandOff(r)
                             \/ Adopt(r) \/ StructRelease(r)
    \/ Checkpoint \/ Rollback \/ EndScope
    \/ (Done /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars

\* ---- properties ----
TypeOK == \A r \in Ids : refs[r].kind \in Kinds
OneEntryPerBlock == \A a \in Addrs : Cardinality(Entries(a)) <= 1
NoReleaseUnderAHolder == ~early
NoUnderflow == ~underflow
RollbackReleasesWhatFollows == ~missed
RollbackKeepsWhatPrecedes == ~overshot
NoLeak == Done => \A r \in Ids : ~refs[r].live
================================================================================
