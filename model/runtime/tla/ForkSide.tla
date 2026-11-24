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
(* stream. A fold's owner checks out the lowest FREE slot under the entry  *)
(* lock (BUSY, held by the thread), or makes one when none is FREE, steps  *)
(* it unlocked and stores it back under the lock into the slot the thread  *)
(* holds, failing late once the native pointer is null; a thread that      *)
(* already holds a BUSY slot is refused. Take in the owner CASes           *)
(* to TAKEN (waiting out SWITCHING), drains the list, and closes the side  *)
(* and appends it (gather); for a fold the CAS also nulls the native       *)
(* pointer (the seal), the side is replayed through step into a value of  *)
(* its own combined after the slots, and a BUSY or FAILED slot fails the  *)
(* fold. FAILED (the entry state) fails it. Take in *)
(* a non-owner fails and closes nothing. Ownership is the fork generation, *)
(* never the pid.                                                          *)
(* A side write is one atomic step: its ordering and wholeness are         *)
(* StreamQueue's.                                                          *)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS Variant, Mode, MaxPuts, MaxForks, ChildForks, OpenMayFail, StepMayFail,
          PutMayFail
\* Mode: "gather" (native prefix ++ side) or "fold" (owner checks out a
\*   free slot; side replayed through step; slots combined in index order).
\* StepMayFail: a fold's step may throw, dropping the checked-out value.
\* PutMayFail: a put may raise before it writes (a batch that does not
\*   encode); the binding then fails the sink: the owner flags its entry, a
\*   child writes a POISON record to the side file.
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
\* "reduce_ignores_busy": the reduce combines a BUSY or FAILED slot's
\*   moved-from value.
\* "reentry_no_busy": checkout does not refuse a thread holding a BUSY slot,
\*   so a re-entrant put by the same thread steps the moved-from value in
\*   that slot and the outer store overwrites it.
\* "slot_after_take": take counts the slots, then seals in a later step,
\*   so a slot made in between is not combined.
\* "replay_skipped": the side records are not replayed through step.
\* "replay_over_busy": a take that replays a side file clears the slots'
\*   marks, so a BUSY or FAILED slot does not fail the fold.
\* "failed_put_unmarked": a put that raises does not fail its sink, so a
\*   producer that swallows the error gets a take without the batch.
\* "checkout_by_thread": a slot belongs to the thread that made it, which
\*   checks it out again on every put, so a sequential producer whose puts
\*   land on different threads is combined out of order, and the slots grow
\*   with the threads that ever put.
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

\* A fold value: its own batches, or a reference to the shared init object.
Fresh == [ref |-> Variant = "shared_init", v |-> <<>>]
NoValue == [ref |-> FALSE, v |-> <<>>]
\* The side record a failed put writes in a child (SIDE-10).
Poison == 0

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
    slot,       \* proc -> sequence of [st: "FREE"|"BUSY"|"FAILED", holder, ref, v]
    held,       \* thread -> the value it checked out
    initObj,    \* the shared init object (only "shared_init" writes it)
    counted,    \* the slot indices take will combine
    sideV,      \* the side records replayed through step from a fresh init
    putFailed,  \* the owner's entry was flagged by a put that raised
    badOk,      \* a fold put returned success after the seal
    overlap,    \* ghost: two fold puts were ever in flight at once
    peak        \* ghost: most slots ever checked out (BUSY or FAILED) at once

vars == <<st, nat, drained, count, alive, born, pid, sideSt, side, closedBy,
          openFailed, pc, cur, snap, kid, did, waited, know, res, before,
          nextId, tk, takeRes, result, natPtr, slot, held, initObj, counted,
          sideV, putFailed, badOk, overlap, peak>>
procVars == <<st, nat, drained, count, alive, born, pid>>
sideVars == <<sideSt, side, closedBy, openFailed, putFailed>>
threadVars == <<pc, cur, snap, kid, did, waited, know>>
putVars == <<res, before, nextId>>
takeVars == <<tk, takeRes, result>>
foldVars == <<natPtr, slot, held, initObj, counted, sideV, badOk, overlap, peak>>

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
    /\ slot = [p \in Procs |-> <<>>]
    /\ held = [t \in Threads |-> NoValue]
    /\ initObj = <<>>
    /\ counted = {}
    /\ sideV = NoValue
    /\ putFailed = FALSE
    /\ badOk = FALSE
    /\ overlap = FALSE
    /\ peak = 0

\* ---- gather start (the entry is made) ----

Start(t) ==
    /\ t = "t1" /\ pc[t] = "Idle" /\ st["P"] = "none"
    /\ count["P"] = 0 \/ Variant = "born_in_window"
    /\ st' = [st EXCEPT !["P"] = "NATIVE"]
    /\ natPtr' = [natPtr EXCEPT !["P"] = TRUE]
    /\ UNCHANGED sideSt
    /\ UNCHANGED <<nat, drained, count, alive, born, pid, side, closedBy,
                   openFailed, putFailed>>
    /\ UNCHANGED <<threadVars, putVars, takeVars>>
    /\ UNCHANGED <<slot, held, initObj, counted, sideV, badOk, overlap, peak>>

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
    /\ \/ /\ IF r \in {"late", "lost"}
             THEN /\ res' = [res EXCEPT ![id] = r]
                  /\ UNCHANGED pc
             ELSE /\ res' = [res EXCEPT ![id] = "pending"]
                  /\ pc' = [pc EXCEPT ![t] =
                        IF r = "side" THEN "SideWrite"
                        ELSE IF Mode = "fold" THEN "Checkout" ELSE "Append"]
          /\ UNCHANGED <<side, putFailed>>
       \/ /\ PutMayFail /\ r \in {"native", "side"}
          \* A child's put that fails once the side file is closed is late:
          \* the take it would have joined has returned.
          /\ res' = [res EXCEPT ![id] =
                IF ~IsOwner(p) /\ sideSt /= "open" THEN "late" ELSE "failed"]
          /\ UNCHANGED pc
          /\ IF Variant = "failed_put_unmarked" THEN UNCHANGED <<side, putFailed>>
             ELSE IF IsOwner(p)
             THEN /\ putFailed' = TRUE /\ UNCHANGED side
             ELSE /\ side' = IF sideSt = "open" THEN Append(side, Poison) ELSE side
                  /\ UNCHANGED putFailed
    /\ overlap' = (overlap \/ \E j \in Ids : res[j] = "pending")
    /\ UNCHANGED <<procVars, sideSt, closedBy, openFailed, snap, kid, did, waited,
                   know, takeVars, natPtr, slot, held, initObj, counted, sideV, badOk,
                   peak>>

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
    /\ UNCHANGED <<procVars, sideSt, closedBy, openFailed, putFailed, cur, snap, kid, did,
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
    /\ UNCHANGED <<nat, drained, count, alive, born, pid, side, closedBy, putFailed,
                   cur, snap, kid, did, waited, know, putVars, takeVars, foldVars>>

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
                   held, initObj, counted, sideV, badOk, overlap, peak>>

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

\* A sequential producer in another process hands its next put to another
\* of the owner's worker threads: the returned puts it knows of become
\* that thread's causal past.
Pass(t) ==
    /\ Mode = "fold" /\ t \in PThreads /\ pc[t] = "Idle"
    /\ \E u \in PThreads \ {t} :
         /\ pc[u] = "Idle" /\ ~(know[t] \subseteq know[u])
         /\ know' = [know EXCEPT ![u] = @ \cup know[t]]
    /\ UNCHANGED <<procVars, sideVars, pc, cur, snap, kid, did, waited, putVars,
                   takeVars, foldVars>>

\* ---- fold put: checkout, step, store ----

Deref(x) == IF x.ref THEN initObj ELSE x.v

Idx(s) == 1..Len(s)
Held(s, t) == {i \in Idx(s) : s[i].holder = t}
FreeIdx(s) == {i \in Idx(s) : s[i].st = "FREE"}
Min(S) == CHOOSE i \in S : \A j \in S : i <= j
Out(s) == Cardinality({i \in Idx(s) : s[i].st /= "FREE"})
Busy(x, t) == [st |-> "BUSY", holder |-> t, ref |-> FALSE, v |-> <<>>]
\* checkout_by_thread keeps the slot's holder after the store.
FreedBy(t) == IF Variant = "checkout_by_thread" THEN t ELSE "none"

\* Under the entry lock: phase 1. The slot a put takes: the one its thread
\* holds (checkout_by_thread), else the lowest FREE one, else a new one.
Target(s, t) ==
    IF Variant = "checkout_by_thread" /\ Held(s, t) /= {} THEN Min(Held(s, t))
    ELSE IF Variant /= "checkout_by_thread" /\ FreeIdx(s) /= {} THEN Min(FreeIdx(s))
    ELSE Len(s) + 1

Checkout(t) ==
    LET p == ProcOf(t) id == cur[t] s == slot[p] i == Target(s, t) IN
    /\ pc[t] = "Checkout"
    /\ IF ~natPtr[p]
       THEN /\ res' = [res EXCEPT ![id] = "late"]
            /\ pc' = [pc EXCEPT ![t] = "Idle"]
            /\ UNCHANGED <<slot, held, peak>>
       ELSE IF \E j \in Idx(s) : s[j].st = "FAILED"
       THEN /\ res' = [res EXCEPT ![id] = "failed"]
            /\ pc' = [pc EXCEPT ![t] = "Idle"]
            /\ UNCHANGED <<slot, held, peak>>
       ELSE IF \E j \in Held(s, t) : s[j].st = "BUSY"
       THEN /\ res' = [res EXCEPT ![id] = "reentrant"]
            /\ pc' = [pc EXCEPT ![t] = "Idle"]
            /\ UNCHANGED <<slot, held, peak>>
       ELSE /\ held' = [held EXCEPT ![t] =
                  IF i > Len(s) THEN Fresh ELSE [ref |-> s[i].ref, v |-> s[i].v]]
            /\ slot' = [slot EXCEPT ![p] =
                  IF i > Len(s) THEN Append(s, Busy(s, t)) ELSE [s EXCEPT ![i] = Busy(s, t)]]
            /\ peak' = IF Out(slot'[p]) > peak THEN Out(slot'[p]) ELSE peak
            /\ pc' = [pc EXCEPT ![t] = "Step"]
            /\ UNCHANGED res
    /\ UNCHANGED <<procVars, sideVars, cur, snap, kid, did, waited, know, before,
                   nextId, takeVars, natPtr, initObj, counted, sideV, badOk, overlap>>

\* The slot a thread's put has checked out.
Mine(s, t) == Min({i \in Held(s, t) : s[i].st = "BUSY"})
HasMine(s, t) == \E i \in Held(s, t) : s[i].st = "BUSY"

\* No lock: phase 2. The value is exclusive to the thread, so the step may
\* mutate it in place; a reference to the shared init mutates that object.
\* A step that throws marks its slot FAILED, under the lock.
StepIt(t) ==
    LET p == ProcOf(t) id == cur[t] x == held[t] IN
    /\ pc[t] = "Step"
    /\ \/ /\ pc' = [pc EXCEPT ![t] = "Store"]
          /\ IF x.ref THEN /\ initObj' = Append(initObj, id) /\ UNCHANGED held
                      ELSE /\ held' = [held EXCEPT ![t].v = Append(@, id)]
                           /\ UNCHANGED initObj
          /\ UNCHANGED <<res, slot>>
       \/ /\ StepMayFail
          /\ res' = [res EXCEPT ![id] = "failed"]
          /\ held' = [held EXCEPT ![t] = NoValue]
          /\ slot' = IF natPtr[p] /\ HasMine(slot[p], t)
                     THEN [slot EXCEPT ![p][Mine(slot[p], t)].st = "FAILED"]
                     ELSE slot
          /\ pc' = [pc EXCEPT ![t] = "Idle"]
          /\ UNCHANGED initObj
    /\ UNCHANGED <<procVars, sideVars, cur, snap, kid, did, waited, know, before,
                   nextId, takeVars, natPtr, counted, sideV, badOk, overlap, peak>>

\* A sink the user's step calls on its own thread (a stashed handle): one
\* whole nested put, under the lock for its phases 1 and 3.
ReEnter(t) ==
    LET p == ProcOf(t) id == nextId s == slot[p] IN
    /\ pc[t] = "Step" /\ nextId <= MaxPuts /\ "reenter" \notin did[t]
    /\ did' = [did EXCEPT ![t] = @ \cup {"reenter"}]
    /\ nextId' = nextId + 1
    /\ before' = [before EXCEPT ![id] = know[t]]
    /\ overlap' = TRUE
    /\ IF ~natPtr[p]
       THEN /\ res' = [res EXCEPT ![id] = "late"]
            /\ UNCHANGED <<slot, initObj, know>>
       ELSE IF HasMine(s, t) /\ Variant /= "reentry_no_busy"
       THEN /\ res' = [res EXCEPT ![id] = "reentrant"]
            /\ UNCHANGED <<slot, initObj, know>>
       ELSE IF HasMine(s, t)
       THEN \* reentry_no_busy: the nested put steps the moved-from value in
            \* the outer put's slot, which the outer store then overwrites.
            /\ slot' = [slot EXCEPT ![p][Mine(s, t)].v = Append(@, id)]
            /\ UNCHANGED initObj
            /\ Returned(t, id)
       ELSE /\ UNCHANGED <<slot, initObj, know>>
            /\ res' = [res EXCEPT ![id] = "reentrant"]
    /\ UNCHANGED <<procVars, sideVars, pc, cur, snap, kid, waited, takeVars, natPtr,
                   held, counted, sideV, badOk, peak>>

\* Under the entry lock: phase 3, into the slot the thread holds. A late
\* store drops the value after unlocking.
Store(t) ==
    LET p == ProcOf(t) id == cur[t] x == held[t] s == slot[p] IN
    /\ pc[t] = "Store"
    /\ IF ~natPtr[p] /\ Variant \notin {"store_ignores_seal", "append_ignores_taken"}
       THEN /\ res' = [res EXCEPT ![id] = "late"]
            /\ UNCHANGED <<slot, know, badOk>>
       ELSE /\ slot' = [slot EXCEPT ![p][Mine(s, t)] =
                  [st |-> "FREE", holder |-> FreedBy(t), ref |-> x.ref, v |-> x.v]]
            /\ Returned(t, id)
            /\ badOk' = (badOk \/ ~natPtr[p] \/ takeRes /= "none")
    /\ held' = [held EXCEPT ![t] = NoValue]
    /\ pc' = [pc EXCEPT ![t] = "Idle"]
    /\ UNCHANGED <<procVars, sideVars, cur, snap, kid, did, waited, before, nextId,
                   takeVars, natPtr, initObj, counted, sideV, overlap, peak>>

\* ---- take ----

TakeBegin(t) ==
    LET s == st["P"] IN
    /\ t = "t1" /\ pc[t] = "Idle" /\ takeRes = "none" /\ "take" \notin did[t]
    /\ s \notin {"none", "TAKEN"}
    /\ s /= "SWITCHING" \/ Variant = "take_skips_cas"
    /\ did' = [did EXCEPT ![t] = @ \cup {"take"}]
    /\ IF (s = "FAILED" /\ Variant /= "take_ignores_failed") \/ putFailed
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
               THEN /\ counted' = Idx(slot["P"])
                    /\ natPtr' = IF Variant = "slot_after_take" THEN natPtr
                                 ELSE [natPtr EXCEPT !["P"] = FALSE]
               ELSE UNCHANGED <<counted, natPtr>>
            /\ UNCHANGED takeRes
    /\ UNCHANGED <<nat, drained, count, alive, born, pid, sideVars, cur, snap,
                   kid, waited, know, putVars, result, slot, held, initObj, sideV,
                   badOk, overlap, peak>>

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
            /\ IF tk = "SIDE" /\ Poison \in Range(side)
               THEN /\ takeRes' = "failed"
                    /\ UNCHANGED st
               ELSE TakeEnd
            /\ UNCHANGED natPtr
       ELSE /\ natPtr' = [natPtr EXCEPT !["P"] = FALSE]
            /\ pc' = [pc EXCEPT ![t] = "TakeReplay"]
            /\ UNCHANGED <<st, takeRes, result>>
    /\ UNCHANGED <<nat, drained, count, alive, born, pid, side, openFailed, putFailed, cur, snap,
                   kid, did, waited, know, putVars, tk, slot, held, initObj,
                   counted, sideV, badOk, overlap, peak>>

\* The side records, each through step, into a value of their own that
\* starts from init. The seal holds every other put out of the slot table.
TakeReplay(t) ==
    LET recs == IF tk = "SIDE" /\ Variant /= "replay_skipped" THEN side ELSE <<>> IN
    /\ pc[t] = "TakeReplay"
    /\ IF Fresh.ref
       THEN /\ initObj' = initObj \o recs
            /\ sideV' = [ref |-> recs /= <<>>, v |-> <<>>]
       ELSE /\ sideV' = [ref |-> FALSE, v |-> recs]
            /\ UNCHANGED initObj
    /\ pc' = [pc EXCEPT ![t] = "TakeCombine"]
    /\ UNCHANGED <<procVars, sideVars, cur, snap, kid, did, waited, know, putVars,
                   takeVars, natPtr, slot, held, counted, badOk, overlap, peak>>

\* The slots in index order, then the replayed side.
RECURSIVE CatSlots(_, _)
CatSlots(i, acc) ==
    IF i > Len(slot["P"]) THEN acc
    ELSE CatSlots(i + 1, IF i \in counted THEN acc \o Deref(slot["P"][i]) ELSE acc)

\* A slot still BUSY (its put outlived the take) or FAILED fails the fold;
\* replay_over_busy skips the check when the take replays a side file.
Unsettled ==
    \E i \in counted : slot["P"][i].st /= "FREE"

TakeCombine(t) ==
    /\ pc[t] = "TakeCombine"
    /\ IF (Unsettled /\ Variant /= "reduce_ignores_busy"
            /\ ~(Variant = "replay_over_busy" /\ tk = "SIDE"))
          \/ (tk = "SIDE" /\ Poison \in Range(side))
       THEN /\ takeRes' = "failed"
            /\ UNCHANGED result
       ELSE /\ takeRes' = "ok"
            /\ result' = CatSlots(1, <<>>) \o Deref(sideV)
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
    /\ UNCHANGED <<procVars, side, openFailed, putFailed, pc, cur, snap, kid, waited, know,
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
    \/ ForkBegin(t) \/ Prep(t) \/ Open(t) \/ ForkNow(t) \/ Reap(t) \/ Pass(t)
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
        \A a, b \in Range(result) \cap Ids : a \in before[b] => Pos(result, a) < Pos(result, b)

\* A put that failed for any reason but lateness, or a failed side open,
\* fails the take: no batch is lost silently.
ALostPutFailsTheTake == takeRes = "ok" => \A i \in Ids : res[i] /= "lost"
AFailedOpenFailsTheTake == takeRes = "ok" => ~openFailed

\* A fold put never returns success once the native pointer is null.
AFoldPutNeverReturnsAfterTheSeal == ~badOk

\* A step that threw lost its slot's earlier batches, and a put that raised
\* lost its batch: the take fails, even when the producer swallowed the
\* error.
AFailedStepFailsTheFold == takeRes = "ok" => \A i \in Ids : res[i] /= "failed"

\* STRM-5: a sequential producer (no two puts in flight, no forked child's
\* batch) gets its batches combined in the order it put them, wherever
\* each put was served.
AFoldKeepsASequentialProducersOrder ==
    (Mode = "fold" /\ takeRes = "ok" /\ ~overlap /\ side = <<>>) =>
        \A a, b \in Range(result) \cap Ids : a \in before[b] => Pos(result, a) < Pos(result, b)

\* A slot is made only when every slot is checked out, so the table never
\* holds more slots than puts were ever in flight at once.
AFoldHasNoMoreSlotsThanConcurrentPuts ==
    Mode = "fold" => Len(slot["P"]) <= peak

AChildNeverClosesTheSide == closedBy \in {"none", "P"}

\* No child inherits an entry its parent owns in NATIVE or SWITCHING.
AForkedChildSeesASettledEntry ==
    \A c \in Kids : alive[c] \in {"live", "exited"} => born[c] \notin {"NATIVE", "SWITCHING"}

=============================================================================
