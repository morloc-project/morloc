------------------------------ MODULE ChannelWait ------------------------------
(* A channel's reader and writer waiting on each other (model/runtime/streams.md *)
(* SLOT-21). Every change to the channel (a batch queued, a batch taken, the  *)
(* end) is made under the slot lock and bumps a sequence word; it wakes the   *)
(* sleepers only when some are registered. A side that must wait reads the   *)
(* word and registers itself under the lock, releases the lock, and sleeps    *)
(* only while the word still holds what it read (a futex wait: it returns at  *)
(* once if the word changed, and otherwise until a wake reaches it).          *)
EXTENDS Naturals

CONSTANT Variant, Depth, Batches
\* "design": the design.
\* "read_after_unlock": the waiter reads the word after releasing the lock.
\* "no_bump": taking a batch does not bump the word.
\* "no_register": a waiter sleeps without registering, so no wake reaches it.

Sides == {"w", "r"}

VARIABLES
    lock,      \* "none" | a side
    queue,     \* batches queued
    sent,      \* batches the writer has queued
    got,       \* batches the reader has taken
    done,      \* the writer has ended the channel
    seq,       \* the sequence word
    waiting,   \* registered sleepers
    woken,     \* sleepers a wake has released
    pc,        \* side -> its step
    seen       \* side -> the word as it read it

vars == <<lock, queue, sent, got, done, seq, waiting, woken, pc, seen>>

Init ==
    /\ lock = "none" /\ queue = 0 /\ sent = 0 /\ got = 0 /\ done = FALSE
    /\ seq = 0 /\ waiting = {} /\ woken = {}
    /\ pc = [s \in Sides |-> "lock"] /\ seen = [s \in Sides |-> 0]

\* A change under the lock: bump the word (unless the variant skips it) and,
\* when anyone is registered, wake the sleepers. A wake reaches only a side
\* already blocked; one registered but not yet blocked is saved by the word.
Change(bump) ==
    /\ seq' = IF bump THEN seq + 1 ELSE seq
    /\ woken' = woken \cup {s \in waiting : pc[s] = "blocked"}
    /\ UNCHANGED waiting

\* Under the lock, a side that must wait reads the word and registers.
Wait(s) ==
    /\ seen' = IF Variant = "read_after_unlock" THEN seen ELSE [seen EXCEPT ![s] = seq]
    /\ waiting' = IF Variant = "no_register" THEN waiting ELSE waiting \cup {s}
    /\ lock' = "none"
    /\ pc' = [pc EXCEPT ![s] = IF Variant = "read_after_unlock" THEN "read" ELSE "sleep"]

Lock(s) ==
    /\ pc[s] = "lock" /\ lock = "none"
    /\ lock' = s /\ pc' = [pc EXCEPT ![s] = "check"]
    /\ UNCHANGED <<queue, sent, got, done, seq, waiting, woken, seen>>

WCheck ==
    /\ pc["w"] = "check"
    /\ IF sent = Batches
       THEN /\ done' = TRUE /\ Change(TRUE)
            /\ lock' = "none" /\ pc' = [pc EXCEPT !["w"] = "end"]
            /\ UNCHANGED <<queue, sent, seen>>
       ELSE IF queue < Depth
       THEN /\ queue' = queue + 1 /\ sent' = sent + 1 /\ Change(TRUE)
            /\ lock' = "none" /\ pc' = [pc EXCEPT !["w"] = "lock"]
            /\ UNCHANGED <<done, seen>>
       ELSE /\ Wait("w") /\ UNCHANGED <<queue, sent, done, seq, woken>>
    /\ UNCHANGED got

RCheck ==
    /\ pc["r"] = "check"
    /\ IF queue > 0
       THEN /\ queue' = queue - 1 /\ got' = got + 1 /\ Change(Variant # "no_bump")
            /\ lock' = "none" /\ pc' = [pc EXCEPT !["r"] = "lock"]
            /\ UNCHANGED seen
       ELSE IF done
       THEN /\ lock' = "none" /\ pc' = [pc EXCEPT !["r"] = "end"]
            /\ UNCHANGED <<queue, got, seq, waiting, woken, seen>>
       ELSE /\ Wait("r") /\ UNCHANGED <<queue, got, seq, woken>>
    /\ UNCHANGED <<sent, done>>

\* The variant's late read of the word, outside the lock.
Read(s) ==
    /\ pc[s] = "read"
    /\ seen' = [seen EXCEPT ![s] = seq] /\ pc' = [pc EXCEPT ![s] = "sleep"]
    /\ UNCHANGED <<lock, queue, sent, got, done, seq, waiting, woken>>

\* The futex wait: return at once if the word moved, else block.
Sleep(s) ==
    /\ pc[s] = "sleep"
    /\ IF seq # seen[s]
       THEN /\ pc' = [pc EXCEPT ![s] = "lock"]
            /\ waiting' = waiting \ {s} /\ woken' = woken \ {s}
       ELSE /\ pc' = [pc EXCEPT ![s] = "blocked"] /\ UNCHANGED <<waiting, woken>>
    /\ UNCHANGED <<lock, queue, sent, got, done, seq, seen>>

Blocked(s) ==
    /\ pc[s] = "blocked" /\ s \in woken
    /\ pc' = [pc EXCEPT ![s] = "lock"] /\ woken' = woken \ {s} /\ waiting' = waiting \ {s}
    /\ UNCHANGED <<lock, queue, sent, got, done, seq, seen>>

Next ==
    \/ \E s \in Sides : Lock(s) \/ Read(s) \/ Sleep(s) \/ Blocked(s)
    \/ WCheck \/ RCheck
    \/ (pc["w"] = "end" /\ pc["r"] = "end" /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars
        /\ \A s \in Sides : WF_vars(Lock(s)) /\ WF_vars(Read(s)) /\ WF_vars(Sleep(s)) /\ WF_vars(Blocked(s))
        /\ WF_vars(WCheck) /\ WF_vars(RCheck)

\* ---- properties ----
EveryBatchIsRead == <>(pc["r"] = "end" /\ got = Batches)
BoundedQueue == queue <= Depth
================================================================================
