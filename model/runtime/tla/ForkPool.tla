------------------------------- MODULE ForkPool -------------------------------
(* A fork pool (Python, R) whose idle workers accept their own connections  *)
(* pool's listening socket. Only an idle worker accepts; a worker is busy  *)
(* while it waits on another pool, and the coordinator forks a worker when *)
(* every live worker is busy or none is left. Every idle worker polls the  *)
(* listening socket, and one that loses the race to accept polls again. A  *)
(* worker accepts before it considers retiring, and retires only while     *)
(* the backlog is empty. An accept that fails for want of a descriptor     *)
(* leaves the connection queued and the worker idle. A call from a C++     *)
(* caller calls back into the pool while its worker waits (A->B->A), and   *)
(* the readiness probe is one more connection. The pool counts how many    *)
(* waits on another pool are in progress, by a worker or by a process the  *)
(* user's code forked from one (whose worker then waits on it), and forks  *)
(* a worker when that count reaches the number of live workers. Workers    *)
(* retire only if the language allows it (Retires). If the coordinator     *)
(* dies, its workers see the end of a pipe only it held and leave.         *)
EXTENDS Naturals, FiniteSets

CONSTANTS Variant, Wake, Retires, Death
\* Retires: whether idle surplus workers retire (Python) or never do (R).
\* Death: whether the coordinator may die.
\* Wake: "all" (every idle worker is woken by a pending connection) or
\* "exclusive" (one idle worker is woken per arriving connection).
\* Variant:
\* "design": the design.
\* "emfile_exits": a worker whose accept fails for want of a descriptor exits.
\* "retire_before_accept": a worker may retire with connections queued.
\* "no_refork_after_retire": the coordinator does not fork once every worker
\*   has retired.
\* "accept_while_busy": a worker waiting on another pool accepts.
\* "descendant_uncounted": a wait made by a process forked from a worker is
\*   not counted busy.
\* "no_life_pipe": workers do not see the coordinator's end.

Workers == {"w1", "w2", "w3"}
Calls == {"ping", "call", "back"}

VARIABLES
    ws,        \* worker -> "none" | "importing" | "idle" | "computing" | "busy" | "retiring" | "dead"
    serving,   \* worker -> call or "none"
    backlog,   \* connections queued on the listening socket
    woken,     \* call -> the worker an exclusive wake chose, or "none"
    arrived,   \* calls that have arrived
    done,      \* calls answered
    failedAccept, \* one accept has failed for want of a descriptor
    poolEnded, \* a worker exited without retiring, which ends the pool (SHM-8)
    shutdown,
    descWaiting, \* a process forked from the serving worker waits on another pool
    coordGone  \* the coordinator has died

vars == <<ws, serving, backlog, woken, arrived, done, failedAccept, poolEnded, shutdown, descWaiting, coordGone>>

Live == {w \in Workers : ws[w] \notin {"none", "dead"}}
Busy == {w \in Workers : ws[w] = "busy"}
Idle == {w \in Workers : ws[w] = "idle"}
BusyCount == Cardinality(Busy)
             + (IF descWaiting /\ Variant # "descendant_uncounted" THEN 1 ELSE 0)

Init ==
    /\ ws = [w \in Workers |-> IF w = "w1" THEN "importing" ELSE "none"]
    /\ serving = [w \in Workers |-> "none"]
    /\ backlog = {"ping"}
    /\ woken = [c \in Calls |-> "none"]
    /\ arrived = {"ping"}
    /\ done = {}
    /\ failedAccept = FALSE
    /\ poolEnded = FALSE
    /\ shutdown = FALSE
    /\ descWaiting = FALSE
    /\ coordGone = FALSE

\* A connection arrives; an exclusive wake picks one idle worker, if any.
Arrive(c, wake) ==
    /\ backlog' = backlog \cup {c}
    /\ arrived' = arrived \cup {c}
    /\ woken' = [woken EXCEPT ![c] = wake]

\* The C++ caller's call arrives once the pool has answered the probe.
CallArrives ==
    /\ "ping" \in done
    /\ "call" \notin arrived
    /\ ~shutdown
    /\ \E wake \in (IF Idle = {} THEN {"none"} ELSE Idle) :
         Arrive("call", wake)
    /\ UNCHANGED <<ws, serving, done, failedAccept, poolEnded, shutdown, descWaiting, coordGone>>

\* ---- the coordinator ----

Fork ==
    /\ ~shutdown
    /\ ~poolEnded
    /\ ~coordGone
    /\ \/ Live = {} /\ Variant # "no_refork_after_retire"
       \/ Live # {} /\ BusyCount >= Cardinality(Live)
    /\ \E w \in Workers :
         /\ ws[w] \in {"none", "dead"}
         /\ ws' = [ws EXCEPT ![w] = "importing"]
         /\ serving' = [serving EXCEPT ![w] = "none"]
    /\ UNCHANGED <<backlog, woken, arrived, done, failedAccept, poolEnded, shutdown, descWaiting, coordGone>>

\* ---- workers ----

Imported(w) ==
    /\ ws[w] = "importing"
    /\ ws' = [ws EXCEPT ![w] = "idle"]
    /\ UNCHANGED <<serving, backlog, woken, arrived, done, failedAccept, poolEnded, shutdown, descWaiting, coordGone>>

MayAccept(w, c) ==
    /\ c \in backlog
    /\ \/ ws[w] = "idle"
       \/ Variant = "accept_while_busy" /\ ws[w] = "busy"
    /\ \/ Wake = "all"
       \/ woken[c] \in {w, "none"}

Accept(w) ==
    /\ \E c \in Calls :
         /\ MayAccept(w, c)
         /\ backlog' = backlog \ {c}
         /\ serving' = [serving EXCEPT ![w] = IF ws[w] = "busy" THEN serving[w] ELSE c]
         /\ ws' = IF ws[w] = "busy"
                  THEN ws  \* accepted by a worker that cannot serve it
                  ELSE [ws EXCEPT ![w] = "computing"]
         /\ done' = done
    /\ UNCHANGED <<woken, arrived, failedAccept, poolEnded, shutdown, descWaiting, coordGone>>

\* An accept fails for want of a descriptor, once.
AcceptFails(w) ==
    /\ ~failedAccept
    /\ \E c \in Calls : MayAccept(w, c)
    /\ failedAccept' = TRUE
    /\ IF Variant = "emfile_exits"
       THEN /\ ws' = [ws EXCEPT ![w] = "dead"]
            /\ poolEnded' = TRUE
       ELSE UNCHANGED <<ws, poolEnded, descWaiting, coordGone>>
    /\ UNCHANGED <<serving, backlog, woken, arrived, done, shutdown, descWaiting, coordGone>>

\* The call calls C++, which calls back into the pool: the worker waits.
CallBack(w) ==
    /\ ws[w] = "computing"
    /\ serving[w] = "call"
    /\ "back" \notin arrived
    /\ ws' = [ws EXCEPT ![w] = "busy"]
    /\ \E wake \in (IF Idle = {} THEN {"none"} ELSE Idle) :
         Arrive("back", wake)
    /\ UNCHANGED <<serving, done, failedAccept, poolEnded, shutdown, descWaiting, coordGone>>

\* The call forks a process (a user's process pool) that calls C++, which
\* calls back into the pool; the worker waits on that process.
DescendantCallBack(w) ==
    /\ ws[w] = "computing"
    /\ serving[w] = "call"
    /\ "back" \notin arrived
    /\ descWaiting' = TRUE
    /\ \E wake \in (IF Idle = {} THEN {"none"} ELSE Idle) :
         Arrive("back", wake)
    /\ UNCHANGED <<ws, serving, done, failedAccept, poolEnded, shutdown, coordGone>>

DescendantResumes ==
    /\ descWaiting
    /\ "back" \in done
    /\ descWaiting' = FALSE
    /\ UNCHANGED <<ws, serving, backlog, woken, arrived, done, failedAccept, poolEnded, shutdown, coordGone>>

Resume(w) ==
    /\ ws[w] = "busy"
    /\ "back" \in done
    /\ ws' = [ws EXCEPT ![w] = "computing"]
    /\ UNCHANGED <<serving, backlog, woken, arrived, done, failedAccept, poolEnded, shutdown, descWaiting, coordGone>>

Answer(w) ==
    /\ ws[w] = "computing"
    /\ serving[w] = "call" => ("back" \in arrived /\ ~descWaiting)
    /\ done' = done \cup {serving[w]}
    /\ ws' = [ws EXCEPT ![w] = "idle"]
    /\ serving' = [serving EXCEPT ![w] = "none"]
    /\ UNCHANGED <<backlog, woken, arrived, failedAccept, poolEnded, shutdown, descWaiting, coordGone>>

\* A surplus idle worker retires; in the design only with nothing queued.
Retire(w) ==
    /\ Retires
    /\ ws[w] = "idle"
    /\ Variant = "no_refork_after_retire" \/ Cardinality(Live) > 1
    /\ backlog = {} \/ Variant = "retire_before_accept"
    /\ ws' = [ws EXCEPT ![w] = "dead"]
    /\ UNCHANGED <<serving, backlog, woken, arrived, done, failedAccept, poolEnded, shutdown, descWaiting, coordGone>>

Shutdown ==
    /\ ~shutdown
    /\ Calls \subseteq done
    /\ shutdown' = TRUE
    /\ UNCHANGED <<ws, serving, backlog, woken, arrived, done, failedAccept, poolEnded, descWaiting, coordGone>>

Leave(w) ==
    /\ shutdown \/ (coordGone /\ Variant # "no_life_pipe")
    /\ ws[w] \in {"idle", "importing"}
    /\ ws' = [ws EXCEPT ![w] = "dead"]
    /\ UNCHANGED <<serving, backlog, woken, arrived, done, failedAccept, poolEnded, shutdown, descWaiting, coordGone>>

CoordinatorDies ==
    /\ Death
    /\ ~coordGone
    /\ coordGone' = TRUE
    /\ UNCHANGED <<ws, serving, backlog, woken, arrived, done, failedAccept, poolEnded, shutdown, descWaiting>>

Next ==
    \/ CallArrives
    \/ CoordinatorDies
    \/ DescendantResumes
    \/ Fork
    \/ \E w \in Workers :
         Imported(w) \/ Accept(w) \/ AcceptFails(w) \/ CallBack(w) \/ DescendantCallBack(w)
         \/ Resume(w) \/ Answer(w) \/ Retire(w) \/ Leave(w)
    \/ Shutdown
    \/ UNCHANGED vars

Spec ==
    /\ Init
    /\ [][Next]_vars
    /\ WF_vars(CallArrives)
    /\ WF_vars(Fork)
    /\ WF_vars(DescendantResumes)
    /\ \A w \in Workers :
         /\ WF_vars(Imported(w)) /\ WF_vars(Accept(w)) /\ WF_vars(CallBack(w))
         /\ WF_vars(Resume(w)) /\ WF_vars(Answer(w)) /\ WF_vars(Leave(w))

\* ---- properties ----

\* Every connection that arrives is answered.
EveryCallIsAnswered == \A c \in Calls : c \in arrived ~> c \in done

\* The pool answers its readiness probe.
TheProbeIsAnswered == <>("ping" \in done)

ShutdownEndsTheWorkers == shutdown ~> (\A w \in Workers : ws[w] \in {"none", "dead"})

\* No worker waits for calls once its coordinator is gone.
NoWorkerOutlivesItsCoordinator ==
    coordGone ~> (\A w \in Workers : ws[w] \notin {"idle", "importing"})
================================================================================
