------------------------------- MODULE ForkSide --------------------------------
(* The fork side of an in-pool accumulator (a native gather or a fold      *)
(* cell). The owner process keeps batches in a native list. Its state word *)
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
(* stream. Take in the owner CASes to TAKEN (waiting out SWITCHING), drains *)
(* the list, and closes the side and appends it (gather) or replays it     *)
(* into the list (fold); FAILED fails it. Take in a non-owner fails and    *)
(* closes nothing. Ownership is the fork generation, never the pid.        *)
(* A side write is one atomic step: its ordering and wholeness are         *)
(* StreamQueue's.                                                          *)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS Variant, Mode, MaxPuts, MaxForks, ChildForks, OpenMayFail
\* Mode: "gather" (native prefix ++ side) or "fold" (owner always native,
\*   side replayed into the accumulator; order is not observable).
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
                \*   | "TakeClose" | "Done"
    cur,        \* thread -> put id in flight
    snap,       \* thread -> prepare's snapshot found an entry it owns
    kid,        \* thread -> the child its fork will make
    did,        \* thread -> one-shot ops done: "take", "start"
    waited,     \* thread -> children it has reaped
    know,       \* thread -> returned puts in its causal past
    res,        \* put id -> "none" | "pending" | "ok" | "late" | "lost"
    before,     \* put id -> returned puts in its causal past when it began
    nextId,
    tk,         \* the state word take saw
    takeRes,    \* "none" | "ok" | "failed"
    result

vars == <<st, nat, drained, count, alive, born, pid, sideSt, side, closedBy,
          openFailed, pc, cur, snap, kid, did, waited, know, res, before,
          nextId, tk, takeRes, result>>
procVars == <<st, nat, drained, count, alive, born, pid>>
sideVars == <<sideSt, side, closedBy, openFailed>>
threadVars == <<pc, cur, snap, kid, did, waited, know>>
putVars == <<res, before, nextId>>
takeVars == <<tk, takeRes, result>>

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

\* ---- gather start (the entry is made) ----

Start(t) ==
    /\ t = "t1" /\ pc[t] = "Idle" /\ st["P"] = "none"
    /\ count["P"] = 0 \/ Variant = "born_in_window"
    /\ st' = [st EXCEPT !["P"] = "NATIVE"]
    /\ UNCHANGED sideSt
    /\ UNCHANGED <<nat, drained, count, alive, born, pid, side, closedBy,
                   openFailed>>
    /\ UNCHANGED <<threadVars, putVars, takeVars>>

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
            /\ pc' = [pc EXCEPT ![t] = IF r = "native" THEN "Append" ELSE "SideWrite"]
    /\ UNCHANGED <<procVars, sideVars, snap, kid, did, waited, know, takeVars>>

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
                   kid, did, waited, before, nextId, takeVars>>

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
                   waited, before, nextId, takeVars>>

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
                   know, putVars, takeVars>>

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
                   kid, did, waited, know, putVars, takeVars>>

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
                   snap, kid, did, waited, know, putVars, takeVars>>

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
    /\ pc' = [pc EXCEPT ![t] = "Idle", ![c] = "Idle"]
    /\ UNCHANGED <<sideVars, cur, snap, kid, did, waited, putVars, takeVars>>

\* waitpid: the child's causal past joins the waiter's.
Reap(t) ==
    LET p == ProcOf(t) IN
    /\ pc[t] = "Idle"
    /\ \E c \in KidSet(p) :
         /\ alive[c] = "exited" /\ c \notin waited[t]
         /\ waited' = [waited EXCEPT ![t] = @ \cup {c}]
         /\ know' = [know EXCEPT ![t] = @ \cup know[c]]
    /\ UNCHANGED <<procVars, sideVars, pc, cur, snap, kid, did, putVars, takeVars>>

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
            /\ UNCHANGED <<st, tk>>
       ELSE /\ tk' = IF s = "SIDE" THEN "SIDE" ELSE "NATIVE"
            /\ st' = IF Variant = "take_skips_cas" THEN st
                     ELSE [st EXCEPT !["P"] = "TAKEN"]
            /\ pc' = [pc EXCEPT ![t] = IF Mode = "gather" THEN "TakeDrain" ELSE "TakeClose"]
            /\ UNCHANGED takeRes
    /\ UNCHANGED <<nat, drained, count, alive, born, pid, sideVars, cur, snap,
                   kid, waited, know, putVars, result>>

TakeEnd ==
    /\ takeRes' = "ok"
    /\ st' = IF Variant = "take_skips_cas" THEN [st EXCEPT !["P"] = "TAKEN"] ELSE st

\* Under the list's mutex.
TakeDrain(t) ==
    /\ pc[t] = "TakeDrain"
    /\ drained' = [drained EXCEPT !["P"] = TRUE]
    /\ result' = nat["P"]
    /\ IF Mode = "gather"
       THEN /\ pc' = [pc EXCEPT ![t] = "TakeClose"]
            /\ UNCHANGED <<st, takeRes>>
       ELSE /\ pc' = [pc EXCEPT ![t] = "Idle"]
            /\ TakeEnd
    /\ UNCHANGED <<nat, count, alive, born, pid, sideVars, cur, snap, kid, did,
                   waited, know, putVars, tk>>

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
            /\ UNCHANGED nat
       ELSE /\ nat' = IF tk = "SIDE" THEN [nat EXCEPT !["P"] = @ \o side] ELSE nat
            /\ pc' = [pc EXCEPT ![t] = "TakeDrain"]
            /\ UNCHANGED <<st, takeRes, result>>
    /\ UNCHANGED <<drained, count, alive, born, pid, side, openFailed, cur, snap,
                   kid, did, waited, know, putVars, tk>>

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
                   putVars, takeVars>>

\* A gather starting in a child waits for the child's forking count.
ChildStart(t) ==
    /\ t \in Kids /\ pc[t] = "Idle" /\ "start" \notin did[t]
    /\ did' = [did EXCEPT ![t] = @ \cup {"start"}]
    /\ pc' = [pc EXCEPT ![t] = "StartOwn"]
    /\ UNCHANGED <<procVars, sideVars, cur, snap, kid, waited, know, putVars, takeVars>>

StartOwn(t) ==
    /\ pc[t] = "StartOwn"
    /\ count[ProcOf(t)] = 0
    /\ pc' = [pc EXCEPT ![t] = "Idle"]
    /\ UNCHANGED <<procVars, sideVars, cur, snap, kid, did, waited, know, putVars,
                   takeVars>>

Stop(t) ==
    /\ pc[t] = "Idle"
    /\ pc' = [pc EXCEPT ![t] = "Done"]
    /\ alive' = IF t \in Kids THEN [alive EXCEPT ![t] = "exited"] ELSE alive
    /\ UNCHANGED <<st, nat, drained, count, born, pid, sideVars, cur, snap, kid,
                   did, waited, know, putVars, takeVars>>

Thread(t) ==
    \/ Start(t) \/ BeginPut(t) \/ NatAppend(t) \/ SideWrite(t)
    \/ ForkBegin(t) \/ Prep(t) \/ Open(t) \/ ForkNow(t) \/ Reap(t)
    \/ TakeBegin(t) \/ TakeDrain(t) \/ TakeClose(t)
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

AChildNeverClosesTheSide == closedBy \in {"none", "P"}

\* No child inherits an entry its parent owns in NATIVE or SWITCHING.
AForkedChildSeesASettledEntry ==
    \A c \in Kids : alive[c] \in {"live", "exited"} => born[c] \notin {"NATIVE", "SWITCHING"}

=============================================================================
