------------------------------- MODULE ForkSide --------------------------------
(* The fork side of an in-pool accumulator (a native gather or a fold      *)
(* accumulator). The owner process keeps batches in a native list, or a  *)
(* fold's per-thread slots. Its state word                                 *)
(* moves NATIVE -> SWITCHING -> SIDE | FAILED in the fork prepare handler, *)
(* and NATIVE | SIDE -> TAKEN at take. The                                 *)
(* prepare handler raises the process's forking count, and for an entry   *)
(* its process owns, wins the CAS to SWITCHING, opens the side stream and  *)
(* stores SIDE, or FAILED if the open failed; a prepare that finds another *)
(* prepare's SWITCHING waits for it. After the fork the parent lowers the  *)
(* count and the child starts at zero. An entry is made only while the    *)
(* count is zero. A put routes on the state word: the owner appends to the *)
(* native list, under the list's mutex, failing once take has drained it; *)
(* a gather's owner in SIDE, and any other process, writes to the side     *)
(* stream. A fold's owner checks out its thread's slot under the entry     *)
(* lock (BUSY), steps it unlocked and stores it back under the lock,       *)
(* failing late once the native pointer is null. Take in the owner CASes   *)
(* to TAKEN (waiting out SWITCHING), drains the list, and closes the side  *)
(* and appends it (gather); for a fold the CAS also nulls the native       *)
(* pointer (the seal), the side is replayed through step into the take     *)
(* thread's slot, and a BUSY slot fails the fold. FAILED fails it. Take in *)
(* a non-owner fails and closes nothing. Ownership is the fork generation, *)
(* never the pid.                                                          *)
(* A side write is one atomic step: its ordering and wholeness are         *)
(* StreamQueue's.                                                          *)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS Variant, Mode, MaxPuts, MaxForks, ChildForks, OpenMayFail, StepMayFail
\* Mode: "gather" (native prefix ++ side) or "fold" (owner checks out its
\*   thread's slot; side replayed through step; order is not observable).
\* StepMayFail: a fold's step may throw, dropping the checked-out value.
\* MaxForks: forks by the owner process (1 or 2); ChildForks: 0 or 1.
\* "design": the design.
\* "take_skips_cas": take loads the state word and stores TAKEN at the end,
\*   so a prepare that switches in between opens a side nobody reads.
\* "born_in_window": an entry is made without waiting for the forking count.
\* "take_in_child_closes": a take in a non-owner closes the side stream.
\* "child_sees_switching": a prepare that finds SWITCHING forks without
\*   waiting for it to resolve.
\* "append_ignores_taken": the native append does not check the drain.
\* "route_by_pid": ownership is the pid, which a descendant in another pid
\*   namespace can share with the owner.
\* "open_failure_left_native": a failed side open leaves the entry NATIVE.
\* "take_ignores_failed": take on FAILED returns the native list.
\* "owner_stays_native": a gather's owner keeps appending natively in SIDE.
\* "child_decrements_count": the child lowers its inherited forking count
\*   instead of starting at zero.
\* Fold variants:
\* "store_ignores_seal" (and "append_ignores_taken" in fold mode): the
\*   store-back does not check the native pointer.
\* "reduce_ignores_busy": the reduce combines a BUSY slot's moved-from value.
\* "reentry_no_busy": checkout does not check BUSY, so a re-entrant put by
\*   the same thread steps the moved-from value and the outer store
\*   overwrites it.
\* "slot_after_take": take counts the slots, then seals in a later step,
\*   so a slot made in between is not combined.
\* "replay_skipped": the side records are not replayed through step.
\* "replay_over_busy": the side replay stores into the take thread's slot
\*   even when it is BUSY, clearing the mark.
\* "shared_init": every slot starts from the same init object, and an
\*   in-place step mutates it.

PThreads == {"t1", "t2"}
Kids == {"C1", "C2", "G"}
Threads == PThreads \cup Kids
Procs == {"P"} \cup Kids
Ids == 1..MaxPuts

ProcOf(t) == IF t \in PThreads THEN "P" ELSE t
KidsOf(p) ==
    IF p = "P" THEN IF MaxForks = 2 THEN <<"C1", "C2">> ELSE <<"C1">>
    ELSE IF p \in {"C1", "C2"} /\ ChildForks > 0 THEN <<"G">> ELSE <<>>
KidSet(p) == {KidsOf(p)[i] : i \in 1..Len(KidsOf(p))}

Cnt(s, x) == Cardinality({i \in 1..Len(s) : s[i] = x})
Pos(s, x) == CHOOSE i \in 1..Len(s) : s[i] = x
Range(s) == {s[i] : i \in 1..Len(s)}

ThreadOrder == <<"t1", "t2", "C1", "C2", "G">>
\* A fold value: its own batches, or a reference to the shared init object.
Fresh == [ref |-> Variant = "shared_init", v |-> <<>>]
NoSlot == [st |-> "none", ref |-> FALSE, v |-> <<>>]

VARIABLES
    st,         \* proc -> entry state word in that process's memory
    nat,        \* proc -> native list (only the owner's is ever taken)
    drained,    \* proc -> take has moved the native list out
    count,      \* proc -> forking count
    alive,      \* proc -> "unborn" | "reserved" | "live" | "exited"
    born,       \* proc -> the state word the child inherited
    pid,        \* proc -> the pid the process reports
    sideSt,     \* "none" | "open" | "closed": the side stream (shared)
    side,       \* elements written to the side stream, in lock order
    closedBy,   \* the process that closed the side stream
    openFailed, \* a prepare's side open failed
    pc,         \* thread -> "None" | "Idle" | "Append" | "SideWrite" | "Prep"
                \*   | "Open" | "ForkNow" | "StartOwn" | "TakeDrain"
                \*   | "TakeClose" | "Checkout" | "Step" | "Store"
                \*   | "TakeReplay" | "TakeCombine" | "Done"
    cur,        \* thread -> put id in flight
    snap,       \* thread -> prepare's snapshot found an entry it owns
    kid,        \* thread -> the child its fork will make
    did,        \* thread -> one-shot ops done: "take", "start"
    waited,     \* thread -> children it has reaped
    know,       \* thread -> returned puts in its causal past
    res,        \* put id -> "none" | "pending" | "ok" | "late" | "lost"
                \*   | "reentrant" | "failed"
    before,     \* put id -> returned puts in its causal past when it began
    nextId,
    tk,         \* the state word take saw
    takeRes,    \* "none" | "ok" | "failed"
    result,
    natPtr,     \* proc -> a fold's native pointer is non-null
    slot,       \* proc -> thread -> [st: "none"|"FREE"|"BUSY", ref, v]
    held,       \* thread -> the value it checked out
    initObj,    \* the shared init object (only "shared_init" writes it)
    counted,    \* the slots take will combine
    badOk       \* a fold put returned success after the seal

vars == <<st, nat, drained, count, alive, born, pid, sideSt, side, closedBy,
          openFailed, pc, cur, snap, kid, did, waited, know, res, before,
          nextId, tk, takeRes, result, natPtr, slot, held, initObj, counted,
          badOk>>
procVars == <<st, nat, drained, count, alive, born, pid>>
sideVars == <<sideSt, side, closedBy, openFailed>>
threadVars == <<pc, cur, snap, kid, did, waited, know>>
putVars == <<res, before, nextId>>
takeVars == <<tk, takeRes, result>>
foldVars == <<natPtr, slot, held, initObj, counted, badOk>>

IsOwner(p) == IF Variant = "route_by_pid" THEN pid[p] = "P" ELSE p = "P"

Init ==
    /\ st = [p \in Procs |-> "none"]
    /\ nat = [p \in Procs |-> <<>>]
    /\ drained = [p \in Procs |-> FALSE]
    /\ count = [p \in Procs |-> 0]
    /\ alive = [p \in Procs |-> IF p = "P" THEN "live" ELSE "unborn"]
    /\ born = [p \in Procs |-> "none"]
    /\ pid = [p \in Procs |-> p]
    /\ sideSt = "none"
    /\ side = <<>>
    /\ closedBy = "none"
    /\ openFailed = FALSE
    /\ pc = [t \in Threads |-> IF t \in PThreads THEN "Idle" ELSE "None"]
    /\ cur = [t \in Threads |-> 0]
    /\ snap = [t \in Threads |-> FALSE]
    /\ kid = [t \in Threads |-> "none"]
    /\ did = [t \in Threads |-> {}]
    /\ waited = [t \in Threads |-> {}]
    /\ know = [t \in Threads |-> {}]
    /\ res = [i \in Ids |-> "none"]
    /\ before = [i \in Ids |-> {}]
    /\ nextId = 1
    /\ tk = "none"
    /\ takeRes = "none"
    /\ result = <<>>
    /\ natPtr = [p \in Procs |-> FALSE]
    /\ slot = [p \in Procs |-> [t \in Threads |-> NoSlot]]
    /\ held = [t \in Threads |-> [ref |-> FALSE, v |-> <<>>]]
    /\ initObj = <<>>
    /\ counted = {}
    /\ badOk = FALSE

\* ---- gather start (the entry is made) ----

Start(t) ==
    /\ t = "t1" /\ pc[t] = "Idle" /\ st["P"] = "none"
    /\ count["P"] = 0 \/ Variant = "born_in_window"
    /\ st' = [st EXCEPT !["P"] = "NATIVE"]
    /\ natPtr' = [natPtr EXCEPT !["P"] = TRUE]
    /\ UNCHANGED sideSt
    /\ UNCHANGED <<nat, drained, count, alive, born, pid, side, closedBy,
                   openFailed>>
    /\ UNCHANGED <<threadVars, putVars, takeVars>>
    /\ UNCHANGED <<slot, held, initObj, counted, badOk>>

\* ---- put ----

Route(p, s) ==
    IF s = "TAKEN" THEN "late"
    ELSE IF IsOwner(p) THEN
        IF Mode = "fold" \/ Variant = "owner_stays_native" THEN "native"
        ELSE CASE s \in {"NATIVE", "SWITCHING"} -> "native"
               [] s = "SIDE" -> "side"
               [] OTHER -> "lost"
    ELSE IF s = "SIDE" THEN "side" ELSE "lost"

BeginPut(t) ==
    LET p == ProcOf(t)
        id == nextId
        r == Route(p, st[p])
    IN
    /\ pc[t] = "Idle" /\ nextId <= MaxPuts /\ st[p] /= "none"
    /\ nextId' = nextId + 1
    /\ before' = [before EXCEPT ![id] = know[t]]
    /\ cur' = [cur EXCEPT ![t] = id]
    /\ IF r \in {"late", "lost"}
       THEN /\ res' = [res EXCEPT ![id] = r]
            /\ UNCHANGED pc
       ELSE /\ res' = [res EXCEPT ![id] = "pending"]
            /\ pc' = [pc EXCEPT ![t] =
                  IF r = "side" THEN "SideWrite"
                  ELSE IF Mode = "fold" THEN "Checkout" ELSE "Append"]
    /\ UNCHANGED <<procVars, sideVars, snap, kid, did, waited, know, takeVars,
                   foldVars>>

Returned(t, id) ==
    /\ res' = [res EXCEPT ![id] = "ok"]
    /\ know' = [know EXCEPT ![t] = @ \cup {id}]

\* Under the list's mutex: one step.
NatAppend(t) ==
    LET p == ProcOf(t) id == cur[t] IN
    /\ pc[t] = "Append"
    /\ IF drained[p] /\ Variant /= "append_ignores_taken"
       THEN /\ res' = [res EXCEPT ![id] = "late"]
            /\ UNCHANGED <<nat, know>>
       ELSE /\ nat' = [nat EXCEPT ![p] = Append(@, id)]
            /\ Returned(t, id)
    /\ pc' = [pc EXCEPT ![t] = "Idle"]
    /\ UNCHANGED <<st, drained, count, alive, born, pid, sideVars, cur, snap,
                   kid, did, waited, before, nextId, takeVars, foldVars>>

\* Under the slot lock: one step (StreamQueue).
SideWrite(t) ==
    LET id == cur[t] IN
    /\ pc[t] = "SideWrite"
    /\ CASE sideSt = "open" -> /\ side' = Append(side, id) /\ Returned(t, id)
         [] sideSt = "closed" -> /\ res' = [res EXCEPT ![id] = "late"]
                                 /\ UNCHANGED <<side, know>>
         [] OTHER -> /\ res' = [res EXCEPT ![id] = "lost"]
                     /\ UNCHANGED <<side, know>>
    /\ pc' = [pc EXCEPT ![t] = "Idle"]
    /\ UNCHANGED <<procVars, sideSt, closedBy, openFailed, cur, snap, kid, did,
                   waited, before, nextId, takeVars, foldVars>>

\* ---- fork: prepare, open, fork, after-fork ----

FreeKids(p) == {i \in 1..Len(KidsOf(p)) : alive[KidsOf(p)[i]] = "unborn"}

\* Under the registry lock: raise the count and take the snapshot.
ForkBegin(t) ==
    LET p == ProcOf(t) IN
    /\ pc[t] = "Idle"
    /\ FreeKids(p) /= {}
    /\ LET c == KidsOf(p)[CHOOSE i \in FreeKids(p) : \A j \in FreeKids(p) : i <= j] IN
         /\ alive' = [alive EXCEPT ![c] = "reserved"]
         /\ kid' = [kid EXCEPT ![t] = c]
    /\ count' = [count EXCEPT ![p] = @ + 1]
    /\ snap' = [snap EXCEPT ![t] = st[p] /= "none" /\ IsOwner(p)]
    /\ pc' = [pc EXCEPT ![t] = "Prep"]
    /\ UNCHANGED <<st, nat, drained, born, pid, sideVars, cur, did, waited,
                   know, putVars, takeVars, foldVars>>

Prep(t) ==
    LET p == ProcOf(t) IN
    /\ pc[t] = "Prep"
    /\ ~snap[t] \/ st[p] /= "SWITCHING" \/ Variant = "child_sees_switching"
    /\ IF snap[t] /\ st[p] = "NATIVE"
       THEN /\ st' = [st EXCEPT ![p] = "SWITCHING"]
            /\ pc' = [pc EXCEPT ![t] = "Open"]
       ELSE /\ pc' = [pc EXCEPT ![t] = "ForkNow"]
            /\ UNCHANGED st
    /\ UNCHANGED <<nat, drained, count, alive, born, pid, sideVars, cur, snap,
                   kid, did, waited, know, putVars, takeVars, foldVars>>

Open(t) ==
    LET p == ProcOf(t) IN
    /\ pc[t] = "Open"
    /\ \/ /\ sideSt' = "open"
          /\ st' = [st EXCEPT ![p] = "SIDE"]
          /\ UNCHANGED openFailed
       \/ /\ OpenMayFail
          /\ openFailed' = TRUE
          /\ st' = [st EXCEPT ![p] =
                IF Variant = "open_failure_left_native" THEN "NATIVE" ELSE "FAILED"]
          /\ UNCHANGED sideSt
    /\ pc' = [pc EXCEPT ![t] = "ForkNow"]
    /\ UNCHANGED <<nat, drained, count, alive, born, pid, side, closedBy, cur,
                   snap, kid, did, waited, know, putVars, takeVars, foldVars>>

ForkNow(t) ==
    LET p == ProcOf(t) c == kid[t] IN
    /\ pc[t] = "ForkNow"
    /\ alive' = [alive EXCEPT ![c] = "live"]
    /\ st' = [st EXCEPT ![c] = st[p]]
    /\ nat' = [nat EXCEPT ![c] = nat[p]]
    /\ drained' = [drained EXCEPT ![c] = drained[p]]
    /\ count' = [count EXCEPT ![p] = @ - 1, ![c] =
          IF Variant = "child_decrements_count" THEN count[p] - 1 ELSE 0]
    /\ born' = [born EXCEPT ![c] = st[p]]
    /\ \E q \in (IF Variant = "route_by_pid" THEN {c, "P"} ELSE {c}) :
         pid' = [pid EXCEPT ![c] = q]
    /\ know' = [know EXCEPT ![c] = know[t]]
    /\ natPtr' = [natPtr EXCEPT ![c] = natPtr[p]]
    /\ slot' = [slot EXCEPT ![c] = slot[p]]
    /\ pc' = [pc EXCEPT ![t] = "Idle", ![c] = "Idle"]
    /\ UNCHANGED <<sideVars, cur, snap, kid, did, waited, putVars, takeVars,
                   held, initObj, counted, badOk>>

\* waitpid: the child's causal past joins the waiter's.
Reap(t) ==
    LET p == ProcOf(t) IN
    /\ pc[t] = "Idle"
    /\ \E c \in KidSet(p) :
         /\ alive[c] = "exited" /\ c \notin waited[t]
         /\ waited' = [waited EXCEPT ![t] = @ \cup {c}]
         /\ know' = [know EXCEPT ![t] = @ \cup know[c]]
    /\ UNCHANGED <<procVars, sideVars, pc, cur, snap, kid, did, putVars, takeVars,
                   foldVars>>

\* ---- fold put: checkout, step, store ----

Deref(x) == IF x.ref THEN initObj ELSE x.v

\* Under the entry lock: phase 1.
Checkout(t) ==
    LET p == ProcOf(t) id == cur[t] s == slot[p][t] IN
    /\ pc[t] = "Checkout"
    /\ IF ~natPtr[p]
       THEN /\ res' = [res EXCEPT ![id] = "late"]
            /\ pc' = [pc EXCEPT ![t] = "Idle"]
            /\ UNCHANGED <<slot, held>>
       ELSE IF s.st = "BUSY"
       THEN /\ res' = [res EXCEPT ![id] = "reentrant"]
            /\ pc' = [pc EXCEPT ![t] = "Idle"]
            /\ UNCHANGED <<slot, held>>
       ELSE /\ held' = [held EXCEPT ![t] =
                  IF s.st = "none" THEN Fresh ELSE [ref |-> s.ref, v |-> s.v]]
            /\ slot' = [slot EXCEPT ![p][t] = [st |-> "BUSY", ref |-> FALSE, v |-> <<>>]]
            /\ pc' = [pc EXCEPT ![t] = "Step"]
            /\ UNCHANGED res
    /\ UNCHANGED <<procVars, sideVars, cur, snap, kid, did, waited, know, before,
                   nextId, takeVars, natPtr, initObj, counted, badOk>>

\* No lock: phase 2. The value is exclusive to the thread, so the step may
\* mutate it in place; a reference to the shared init mutates that object.
StepIt(t) ==
    LET id == cur[t] x == held[t] IN
    /\ pc[t] = "Step"
    /\ \/ /\ pc' = [pc EXCEPT ![t] = "Store"]
          /\ IF x.ref THEN /\ initObj' = Append(initObj, id) /\ UNCHANGED held
                      ELSE /\ held' = [held EXCEPT ![t].v = Append(@, id)]
                           /\ UNCHANGED initObj
          /\ UNCHANGED res
       \/ /\ StepMayFail
          /\ res' = [res EXCEPT ![id] = "failed"]
          /\ held' = [held EXCEPT ![t] = [ref |-> FALSE, v |-> <<>>]]
          /\ pc' = [pc EXCEPT ![t] = "Idle"]
          /\ UNCHANGED initObj
    /\ UNCHANGED <<procVars, sideVars, cur, snap, kid, did, waited, know, before,
                   nextId, takeVars, natPtr, slot, counted, badOk>>

\* A sink the user's step calls on its own thread (a stashed handle): one
\* whole nested put, under the lock for its phases 1 and 3.
ReEnter(t) ==
    LET p == ProcOf(t) id == nextId s == slot[p][t]
        base == IF s.st = "none" THEN Fresh ELSE [ref |-> s.ref, v |-> s.v]
    IN
    /\ pc[t] = "Step" /\ nextId <= MaxPuts /\ "reenter" \notin did[t]
    /\ did' = [did EXCEPT ![t] = @ \cup {"reenter"}]
    /\ nextId' = nextId + 1
    /\ before' = [before EXCEPT ![id] = know[t]]
    /\ IF ~natPtr[p]
       THEN /\ res' = [res EXCEPT ![id] = "late"]
            /\ UNCHANGED <<slot, initObj, know>>
       ELSE IF s.st = "BUSY" /\ Variant /= "reentry_no_busy"
       THEN /\ res' = [res EXCEPT ![id] = "reentrant"]
            /\ UNCHANGED <<slot, initObj, know>>
       ELSE /\ IF base.ref
               THEN /\ initObj' = Append(initObj, id)
                    /\ slot' = [slot EXCEPT ![p][t] = [st |-> "FREE", ref |-> TRUE, v |-> <<>>]]
               ELSE /\ slot' = [slot EXCEPT ![p][t] =
                          [st |-> "FREE", ref |-> FALSE, v |-> Append(base.v, id)]]
                    /\ UNCHANGED initObj
            /\ Returned(t, id)
    /\ UNCHANGED <<procVars, sideVars, pc, cur, snap, kid, waited, takeVars, natPtr,
                   held, counted, badOk>>

\* Under the entry lock: phase 3. A late store drops the value after
\* unlocking.
Store(t) ==
    LET p == ProcOf(t) id == cur[t] x == held[t] IN
    /\ pc[t] = "Store"
    /\ IF ~natPtr[p] /\ Variant \notin {"store_ignores_seal", "append_ignores_taken"}
       THEN /\ res' = [res EXCEPT ![id] = "late"]
            /\ UNCHANGED <<slot, know, badOk>>
       ELSE /\ slot' = [slot EXCEPT ![p][t] = [st |-> "FREE", ref |-> x.ref, v |-> x.v]]
            /\ Returned(t, id)
            /\ badOk' = (badOk \/ ~natPtr[p] \/ takeRes /= "none")
    /\ held' = [held EXCEPT ![t] = [ref |-> FALSE, v |-> <<>>]]
    /\ pc' = [pc EXCEPT ![t] = "Idle"]
    /\ UNCHANGED <<procVars, sideVars, cur, snap, kid, did, waited, before, nextId,
                   takeVars, natPtr, initObj, counted>>

\* ---- take ----

TakeBegin(t) ==
    LET s == st["P"] IN
    /\ t = "t1" /\ pc[t] = "Idle" /\ takeRes = "none" /\ "take" \notin did[t]
    /\ s \notin {"none", "TAKEN"}
    /\ s /= "SWITCHING" \/ Variant = "take_skips_cas"
    /\ did' = [did EXCEPT ![t] = @ \cup {"take"}]
    /\ IF s = "FAILED" /\ Variant /= "take_ignores_failed"
       THEN /\ takeRes' = "failed"
            /\ pc' = [pc EXCEPT ![t] = "Idle"]
            /\ IF Mode = "fold"
               THEN /\ st' = [st EXCEPT !["P"] = "TAKEN"]
                    /\ natPtr' = [natPtr EXCEPT !["P"] = FALSE]
               ELSE UNCHANGED <<st, natPtr>>
            /\ UNCHANGED <<tk, counted>>
       ELSE /\ tk' = IF s = "SIDE" THEN "SIDE" ELSE "NATIVE"
            /\ st' = IF Variant = "take_skips_cas" THEN st
                     ELSE [st EXCEPT !["P"] = "TAKEN"]
            /\ pc' = [pc EXCEPT ![t] = IF Mode = "gather" THEN "TakeDrain" ELSE "TakeClose"]
            /\ IF Mode = "fold"
               THEN /\ counted' = {u \in Threads : slot["P"][u].st /= "none"}
                    /\ natPtr' = IF Variant = "slot_after_take" THEN natPtr
                                 ELSE [natPtr EXCEPT !["P"] = FALSE]
               ELSE UNCHANGED <<counted, natPtr>>
            /\ UNCHANGED takeRes
    /\ UNCHANGED <<nat, drained, count, alive, born, pid, sideVars, cur, snap,
                   kid, waited, know, putVars, result, slot, held, initObj, badOk>>

TakeEnd ==
    /\ takeRes' = "ok"
    /\ st' = IF Variant = "take_skips_cas" THEN [st EXCEPT !["P"] = "TAKEN"] ELSE st

\* Under the list's mutex.
TakeDrain(t) ==
    /\ pc[t] = "TakeDrain"
    /\ drained' = [drained EXCEPT !["P"] = TRUE]
    /\ result' = nat["P"]
    /\ pc' = [pc EXCEPT ![t] = "TakeClose"]
    /\ UNCHANGED <<st, takeRes>>
    /\ UNCHANGED <<nat, count, alive, born, pid, sideVars, cur, snap, kid, did,
                   waited, know, putVars, tk, foldVars>>

\* For a fold, the slot_after_take variant seals here, after the count.
TakeClose(t) ==
    /\ pc[t] = "TakeClose"
    /\ IF tk = "SIDE"
       THEN /\ sideSt' = "closed"
            /\ closedBy' = "P"
       ELSE UNCHANGED <<sideSt, closedBy>>
    /\ IF Mode = "gather"
       THEN /\ result' = IF tk = "SIDE" THEN result \o side ELSE result
            /\ pc' = [pc EXCEPT ![t] = "Idle"]
            /\ TakeEnd
            /\ UNCHANGED natPtr
       ELSE /\ natPtr' = [natPtr EXCEPT !["P"] = FALSE]
            /\ pc' = [pc EXCEPT ![t] = "TakeReplay"]
            /\ UNCHANGED <<st, takeRes, result>>
    /\ UNCHANGED <<nat, drained, count, alive, born, pid, side, openFailed, cur, snap,
                   kid, did, waited, know, putVars, tk, slot, held, initObj,
                   counted, badOk>>

\* The side records, each through step, into the take thread's slot. The
\* seal holds every other put out of the slot table. A BUSY slot (the take
\* thread's own step threw earlier) is left BUSY, so the combine fails.
TakeReplay(t) ==
    LET s == slot["P"][t]
        base == IF s.st = "none" THEN Fresh ELSE [ref |-> s.ref, v |-> s.v]
        recs == IF tk = "SIDE" /\ Variant /= "replay_skipped" THEN side ELSE <<>>
    IN
    /\ pc[t] = "TakeReplay"
    /\ CASE s.st = "BUSY" /\ Variant /= "replay_over_busy" ->
              UNCHANGED <<slot, initObj>>
         [] base.ref ->
              /\ initObj' = initObj \o recs
              /\ slot' = [slot EXCEPT !["P"][t] = [st |-> "FREE", ref |-> TRUE, v |-> <<>>]]
         [] OTHER ->
              /\ slot' = [slot EXCEPT !["P"][t] =
                    [st |-> "FREE", ref |-> FALSE, v |-> base.v \o recs]]
              /\ UNCHANGED initObj
    /\ counted' = counted \cup {t}
    /\ pc' = [pc EXCEPT ![t] = "TakeCombine"]
    /\ UNCHANGED <<procVars, sideVars, cur, snap, kid, did, waited, know, putVars,
                   takeVars, natPtr, held, badOk>>

RECURSIVE CatSlots(_, _)
CatSlots(i, acc) ==
    IF i > Len(ThreadOrder) THEN acc
    ELSE LET u == ThreadOrder[i] IN
         CatSlots(i + 1, IF u \in counted THEN acc \o Deref(slot["P"][u]) ELSE acc)

TakeCombine(t) ==
    /\ pc[t] = "TakeCombine"
    /\ IF \E u \in counted : slot["P"][u].st = "BUSY" /\ Variant /= "reduce_ignores_busy"
       THEN /\ takeRes' = "failed"
            /\ UNCHANGED result
       ELSE /\ takeRes' = "ok"
            /\ result' = CatSlots(1, <<>>)
    /\ pc' = [pc EXCEPT ![t] = "Idle"]
    /\ UNCHANGED <<procVars, sideVars, cur, snap, kid, did, waited, know, putVars,
                   tk, foldVars>>

\* A non-owner's take fails. An owner by pid takes its own copy and closes
\* the side.
ChildTake(t) ==
    LET p == ProcOf(t) IN
    /\ t \in Kids /\ pc[t] = "Idle" /\ "take" \notin did[t] /\ st[p] /= "none"
    /\ did' = [did EXCEPT ![t] = @ \cup {"take"}]
    /\ IF (Variant = "take_in_child_closes" \/ IsOwner(p)) /\ sideSt = "open"
       THEN /\ sideSt' = "closed"
            /\ closedBy' = p
       ELSE UNCHANGED <<sideSt, closedBy>>
    /\ UNCHANGED <<procVars, side, openFailed, pc, cur, snap, kid, waited, know,
                   putVars, takeVars, foldVars>>

\* A gather starting in a child waits for the child's forking count.
ChildStart(t) ==
    /\ t \in Kids /\ pc[t] = "Idle" /\ "start" \notin did[t]
    /\ did' = [did EXCEPT ![t] = @ \cup {"start"}]
    /\ pc' = [pc EXCEPT ![t] = "StartOwn"]
    /\ UNCHANGED <<procVars, sideVars, cur, snap, kid, waited, know, putVars, takeVars,
                   foldVars>>

StartOwn(t) ==
    /\ pc[t] = "StartOwn"
    /\ count[ProcOf(t)] = 0
    /\ pc' = [pc EXCEPT ![t] = "Idle"]
    /\ UNCHANGED <<procVars, sideVars, cur, snap, kid, did, waited, know, putVars,
                   takeVars, foldVars>>

Stop(t) ==
    /\ pc[t] = "Idle"
    /\ pc' = [pc EXCEPT ![t] = "Done"]
    /\ alive' = IF t \in Kids THEN [alive EXCEPT ![t] = "exited"] ELSE alive
    /\ UNCHANGED <<st, nat, drained, count, born, pid, sideVars, cur, snap, kid,
                   did, waited, know, putVars, takeVars, foldVars>>

Thread(t) ==
    \/ Start(t) \/ BeginPut(t) \/ NatAppend(t) \/ SideWrite(t)
    \/ ForkBegin(t) \/ Prep(t) \/ Open(t) \/ ForkNow(t) \/ Reap(t)
    \/ Checkout(t) \/ StepIt(t) \/ ReEnter(t) \/ Store(t)
    \/ TakeBegin(t) \/ TakeDrain(t) \/ TakeClose(t) \/ TakeReplay(t) \/ TakeCombine(t)
    \/ ChildTake(t) \/ ChildStart(t) \/ StartOwn(t) \/ Stop(t)

Terminating ==
    /\ \A t \in Threads : pc[t] \in {"None", "Done"}
    /\ UNCHANGED vars

Next == (\E t \in Threads : Thread(t)) \/ Terminating

Spec == Init /\ [][Next]_vars

\* ---- properties ----

OkIds == {i \in Ids : res[i] = "ok"}

\* FS: a put that returned success is in take's result exactly once; a put
\* that returns success after take is impossible.
TakeHoldsEveryReturnedPut ==
    takeRes = "ok" => \A i \in OkIds : Cnt(result, i) = 1
NothingTakenTwice == \A i \in Ids : Cnt(result, i) <= 1

\* Per-thread order, fork and waitpid: a put that began after another
\* returned, in its causal past, comes later in the result. The native
\* prefix before the side stream, and the side stream in lock order, are
\* the cases that matter.
TakeKeepsCausalOrder ==
    (Mode = "gather" /\ takeRes = "ok") =>
        \A a, b \in Range(result) : a \in before[b] => Pos(result, a) < Pos(result, b)

\* A put that failed for any reason but lateness, or a failed side open,
\* fails the take: no batch is lost silently.
ALostPutFailsTheTake == takeRes = "ok" => \A i \in Ids : res[i] /= "lost"
AFailedOpenFailsTheTake == takeRes = "ok" => ~openFailed

\* A fold put never returns success once the native pointer is null.
AFoldPutNeverReturnsAfterTheSeal == ~badOk

\* A step that threw lost its slot's earlier batches: the fold fails.
AFailedStepFailsTheFold == takeRes = "ok" => \A i \in Ids : res[i] /= "failed"

AChildNeverClosesTheSide == closedBy \in {"none", "P"}

\* No child inherits an entry its parent owns in NATIVE or SWITCHING.
AForkedChildSeesASettledEntry ==
    \A c \in Kids : alive[c] \in {"live", "exited"} => born[c] \notin {"NATIVE", "SWITCHING"}

=============================================================================
