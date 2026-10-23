---------------------------- MODULE NamespaceGrow ----------------------------
(* Growing a program's shared memory across processes (model/runtime/shm.md *)
(* SHM-12). Volumes are numbered in order; the primary volume's header      *)
(* holds the next number and a namespace growth lock that reports, rather   *)
(* than poisons on, a holder's death. A process allocates from the volumes  *)
(* it has mapped. One that finds no room maps every initialised volume      *)
(* numbered below the next number and looks again; if there is still none   *)
(* it takes the growth lock, maps and looks once more, and only then        *)
(* creates the volume at the next number, exclusively. It bumps the next    *)
(* number as soon as the create succeeds, then initialises the volume and   *)
(* releases the lock. A name that already exists (its creator died before   *)
(* bumping) is used if initialised and skipped otherwise; it is never       *)
(* removed. A process may also map an initialised volume at any time, as    *)
(* resolving a pointer into it does.                                        *)
EXTENDS Naturals, FiniteSets

CONSTANT Variant
\* "design": the design.
\* "no_lock": growth takes no namespace lock.
\* "no_remap": the grower does not map numbered volumes before creating one.
\* "map_uninit": a process maps a numbered volume whether or not it is
\*   initialised.
\* "bump_after_init": the next number is bumped only once the volume is
\*   initialised, and an existing name is created over.

Procs == {"p1", "p2"}
MaxVol == 4

VARIABLES
    next,      \* the next volume number
    created,   \* volumes whose name exists
    inited,    \* volumes initialised
    creators,  \* volume -> number of processes that created it
    mapped,    \* process -> volumes it has mapped
    full,      \* volumes with no room
    lock,      \* the growth lock's holder, or "none"
    pc,        \* process -> "alloc" | "want" | "locked" | "made" | "done" | "dead"
    made,      \* process -> the volume it is making, or 0
    usedBad    \* a process allocated from a volume not yet initialised

vars == <<next, created, inited, creators, mapped, full, lock, pc, made, usedBad>>

Init ==
    /\ next = 2
    /\ created = {1}
    /\ inited = {1}
    /\ creators = [v \in 1..MaxVol |-> IF v = 1 THEN 1 ELSE 0]
    /\ mapped = [p \in Procs |-> {1}]
    /\ full = {1}
    /\ lock = "none"
    /\ pc = [p \in Procs |-> "alloc"]
    /\ made = [p \in Procs |-> 0]
    /\ usedBad = FALSE

Mappable == IF Variant = "map_uninit" THEN created ELSE inited
Numbered == {v \in 1..MaxVol : v < next}
MapAll(p) == [mapped EXCEPT ![p] = @ \cup (Numbered \cap Mappable)]
HasRoom(p) == \E v \in mapped[p] : v \notin full

Alloc(p) ==
    /\ pc[p] = "alloc"
    /\ IF HasRoom(p)
       THEN /\ \E v \in mapped[p] \ full :
                 /\ usedBad' = (usedBad \/ v \notin inited)
                 /\ full' = full \cup {v}
            /\ pc' = [pc EXCEPT ![p] = "done"]
            /\ UNCHANGED <<next, created, inited, creators, mapped, lock, made>>
       ELSE \* the pass before the lock
            /\ mapped' = IF Variant = "no_remap" THEN mapped ELSE MapAll(p)
            /\ pc' = [pc EXCEPT ![p] = "want"]
            /\ UNCHANGED <<next, created, inited, creators, full, lock, made, usedBad>>

TakeLock(p) ==
    /\ pc[p] = "want"
    /\ IF HasRoom(p)
       THEN /\ pc' = [pc EXCEPT ![p] = "alloc"]
            /\ UNCHANGED <<lock, mapped>>
       ELSE /\ Variant = "no_lock" \/ lock = "none"
            /\ lock' = IF Variant = "no_lock" THEN lock ELSE p
            /\ mapped' = IF Variant = "no_remap" THEN mapped ELSE MapAll(p)
            /\ pc' = [pc EXCEPT ![p] = "locked"]
    /\ UNCHANGED <<next, created, inited, creators, full, made, usedBad>>

\* Under the lock: room after mapping, or create at the next number.
Create(p) ==
    /\ pc[p] = "locked"
    /\ IF HasRoom(p) \/ next > MaxVol
       THEN /\ lock' = IF lock = p THEN "none" ELSE lock
            /\ pc' = [pc EXCEPT ![p] = IF HasRoom(p) THEN "alloc" ELSE "done"]
            /\ UNCHANGED <<next, created, creators, made, mapped>>
       ELSE IF next \in created /\ Variant # "bump_after_init"
       THEN \* a dead creator's name: use it if initialised, else skip it
            /\ next' = next + 1
            /\ mapped' = IF next \in inited THEN [mapped EXCEPT ![p] = @ \cup {next}] ELSE mapped
            /\ UNCHANGED <<created, creators, made, lock, pc>>
       ELSE /\ created' = created \cup {next}
            /\ creators' = [creators EXCEPT ![next] = @ + 1]
            /\ made' = [made EXCEPT ![p] = next]
            /\ next' = IF Variant = "bump_after_init" THEN next ELSE next + 1
            /\ pc' = [pc EXCEPT ![p] = "made"]
            /\ UNCHANGED <<lock, mapped>>
    /\ UNCHANGED <<inited, full, usedBad>>

Publish(p) ==
    /\ pc[p] = "made"
    /\ inited' = inited \cup {made[p]}
    /\ next' = IF Variant = "bump_after_init" THEN made[p] + 1 ELSE next
    /\ mapped' = [mapped EXCEPT ![p] = @ \cup {made[p]}]
    /\ lock' = IF lock = p THEN "none" ELSE lock
    /\ pc' = [pc EXCEPT ![p] = "alloc"]
    /\ UNCHANGED <<created, creators, full, made, usedBad>>

Discover(p) ==
    /\ pc[p] # "dead"
    /\ \E v \in (Numbered \cap Mappable) \ mapped[p] : mapped' = [mapped EXCEPT ![p] = @ \cup {v}]
    /\ UNCHANGED <<next, created, inited, creators, full, lock, pc, made, usedBad>>

\* A process dies; the lock reports the death to its next taker.
Die(p) ==
    /\ pc[p] \in {"locked", "made"}
    /\ pc' = [pc EXCEPT ![p] = "dead"]
    /\ lock' = IF lock = p THEN "none" ELSE lock
    /\ UNCHANGED <<next, created, inited, creators, mapped, full, made, usedBad>>

Next ==
    \/ \E p \in Procs : Alloc(p) \/ TakeLock(p) \/ Create(p) \/ Publish(p) \/ Die(p) \/ Discover(p)
    \/ UNCHANGED vars

Spec == Init /\ [][Next]_vars
        /\ \A p \in Procs : WF_vars(Alloc(p)) /\ WF_vars(TakeLock(p)) /\ WF_vars(Create(p)) /\ WF_vars(Publish(p))

\* ---- properties ----

\* No volume is created while an initialised one still has room.
NoVolumeWhileRoom ==
    \A p \in Procs : pc[p] = "made" => \A v \in inited : v \in full
NeverUsesAnUninitialisedVolume == ~usedBad
OneCreatorPerVolume == \A v \in 1..MaxVol : creators[v] <= 1
EveryAllocationEnds == \A p \in Procs : <>(pc[p] \in {"done", "dead"})
================================================================================
