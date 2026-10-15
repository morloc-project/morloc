------------------------------ MODULE CrossingConn ------------------------------
(* Reused connections into a thread pool. A caller thread caches one       *)
(* connection per callee, tagged with its fork generation; it reuses it    *)
(* only when the last reply said the callee keeps it. The callee pins the  *)
(* worker that served a connection to it: the worker waits for that        *)
(* connection's next request and is not free for others. A request on a    *)
(* new connection still gets a free worker or a new one. A send that fails *)
(* because the callee's end is closed delivered nothing and is retried     *)
(* once on a fresh connection; a closed end after a complete send is the   *)
(* callee's death and is never retried. A forked child closes the cached   *)
(* connections it inherited. Shutdown wakes pinned workers. A connection   *)
(* is tracked for the fork handler only after its socket exists; a fork   *)
(* in between leaves the child a copy it does not close, so the caller    *)
(* drops that socket, which carried nothing yet, and makes another.       *)
EXTENDS Naturals, FiniteSets

CONSTANTS Variant, Faults
\* Faults: which of "fork", "death" and "abandon" may happen in a run.
\* "design": the design.
\* "finishing_counts_pinned": the accept loop counts a pinned worker as free.
\* "child_uses_inherited": a forked child uses its parent's cached connection.
\* "child_keeps_inherited": a forked child keeps its inherited copy open.
\* "retry_after_complete_send": a closed end after a complete send is retried.
\* "keep_after_error": a caller that abandons a call keeps its connection.
\* "join_pinned_without_shutdown_wake": shutdown waits for pinned workers'
\*   connections to close.
\* "keep_across_fork_window": a connection whose socket was made before a
\*   fork and tracked after it is used, so a child holds an untracked copy.

Callers == {"c", "cb", "k"}  \* c: a caller; cb: called back while a worker waits; k: c's forked child
\* As many workers as connections: every connection pins at most one worker,
\* so the pool's growth, unbounded in the code, is never cut short here.
Conns == 1..4
Workers == {"w1", "w2", "w3", "w4"}
MaxCalls == 2
CallsOf(c) == IF c = "c" THEN MaxCalls ELSE 1
None == 0
NoReq == [caller |-> "none", n |-> 0]

VARIABLES
    cst,        \* caller -> "idle" | "sent" | "unborn" | "exited"
    cache,      \* caller -> cached connection or None
    cur,        \* caller -> connection of the call in flight
    calls,      \* caller -> calls completed or failed
    abandoned,  \* a caller gave up on a call without reading its reply
    cs,         \* connection -> "unused" | "open" | "cclosed" | "sclosed" | "closed"
    owner,      \* connection -> caller
    copy,       \* connection -> a forked child still holds a copy of the caller end
    inbox,      \* connection -> request id sent and not yet read, or NoReq
    outbox,     \* connection -> reply id not yet read, or NoReq
    kept,       \* connection -> the callee keeps it
    killed,     \* connection -> its callee end was closed by the pool's death
    queue,      \* new connections waiting for a worker
    ws,         \* worker -> "absent" | "idle" | "serving" | "blocked" | "pinned" | "gone"
    wc,         \* worker -> connection
    held,       \* worker -> request a blocked worker is serving
    runs,       \* request id -> times executed
    pool,       \* "up" | "dead"
    deaths,
    shutdown,
    cbWanted,   \* a worker waits on a callback call
    falseCrash,
    wrongReply,
    tk          \* connection -> [untracked: socket made, not yet tracked;
                \*   across: a fork happened while it was untracked]

vars == <<cst, cache, cur, calls, abandoned, cs, owner, copy, inbox, outbox, kept,
          killed, queue, ws, wc, held, runs, pool, deaths, shutdown, cbWanted,
          falseCrash, wrongReply, tk>>

NoTrack == [untracked |-> FALSE, across |-> FALSE]
Untracked == [untracked |-> TRUE, across |-> FALSE]

Req(c) == [caller |-> c, n |-> calls[c] + 1]
ReqIds == [caller : Callers, n : 1..MaxCalls]

Init ==
    /\ cst = [c \in Callers |-> IF c = "k" THEN "unborn" ELSE "idle"]
    /\ cache = [c \in Callers |-> None]
    /\ cur = [c \in Callers |-> None]
    /\ calls = [c \in Callers |-> 0]
    /\ abandoned = FALSE
    /\ cs = [x \in Conns |-> "unused"]
    /\ owner = [x \in Conns |-> "c"]
    /\ copy = [x \in Conns |-> FALSE]
    /\ inbox = [x \in Conns |-> NoReq]
    /\ outbox = [x \in Conns |-> NoReq]
    /\ kept = [x \in Conns |-> FALSE]
    /\ killed = [x \in Conns |-> FALSE]
    /\ queue = {}
    /\ ws = [w \in Workers |-> IF w = "w1" THEN "idle" ELSE "absent"]
    /\ wc = [w \in Workers |-> None]
    /\ held = [w \in Workers |-> NoReq]
    /\ runs = [r \in ReqIds |-> 0]
    /\ pool = "up"
    /\ deaths = 0
    /\ shutdown = FALSE
    /\ cbWanted = FALSE
    /\ falseCrash = FALSE
    /\ wrongReply = FALSE
    /\ tk = [x \in Conns |-> NoTrack]

CallerOpen(x) == cs[x] \in {"open", "sclosed"}
CalleeOpen(x) == cs[x] \in {"open", "cclosed"}
Fresh == {x \in Conns : cs[x] = "unused"}

\* ---- callers ----

MayCall(c) ==
    /\ cst[c] = "idle"
    /\ calls[c] < CallsOf(c)
    /\ IF c = "cb" THEN cbWanted ELSE TRUE
    /\ ~shutdown

\* Make the socket of a fresh connection; it is tracked and sent in Track.
Connect(c, r) ==
    /\ pool = "up"
    /\ Fresh # {}
    /\ LET x == CHOOSE y \in Fresh : TRUE IN
         /\ cs' = [cs EXCEPT ![x] = "open"]
         /\ owner' = [owner EXCEPT ![x] = c]
         /\ inbox' = [inbox EXCEPT ![x] = r]
         /\ cur' = [cur EXCEPT ![c] = x]
         /\ cst' = [cst EXCEPT ![c] = "connecting"]
         /\ tk' = [tk EXCEPT ![x] = Untracked]

Call(c) ==
    /\ MayCall(c)
    /\ IF cache[c] # None /\ CallerOpen(cache[c])
       THEN LET x == cache[c] IN
         IF cs[x] = "open"
         THEN \* the callee end is open: the request is delivered
              /\ inbox' = [inbox EXCEPT ![x] = Req(c)]
              /\ cur' = [cur EXCEPT ![c] = x]
              /\ cst' = [cst EXCEPT ![c] = "sent"]
              /\ UNCHANGED <<cs, owner, queue, cache, tk>>
         ELSE \* EPIPE: nothing delivered; drop it and retry on a fresh one
              /\ cache' = [cache EXCEPT ![c] = None]
              /\ LET x2 == CHOOSE y \in Fresh : TRUE IN
                   /\ pool = "up"
                   /\ Fresh # {}
                   /\ cs' = [cs EXCEPT ![x] = "closed", ![x2] = "open"]
                   /\ owner' = [owner EXCEPT ![x2] = c]
                   /\ inbox' = [inbox EXCEPT ![x2] = Req(c)]
                   /\ cur' = [cur EXCEPT ![c] = x2]
                   /\ cst' = [cst EXCEPT ![c] = "connecting"]
                   /\ tk' = [tk EXCEPT ![x2] = Untracked]
                   /\ UNCHANGED queue
       ELSE /\ Connect(c, Req(c))
            /\ UNCHANGED <<cache, queue>>
    /\ UNCHANGED <<calls, abandoned, copy, outbox, kept, killed, ws, wc, held, runs,
                   pool, deaths, shutdown, cbWanted, falseCrash, wrongReply>>

\* The socket is tracked, then connected: the request reaches the accept
\* loop. If a fork came between making and tracking it, the socket, which
\* has carried nothing, is dropped and another made; the child's copy of it
\* is never connected.
Track(c) ==
    /\ cst[c] = "connecting"
    /\ pool = "up"
    /\ LET x == cur[c] IN
       IF tk[x].across /\ Variant # "keep_across_fork_window"
       THEN /\ Fresh # {}
            /\ LET x2 == CHOOSE y \in Fresh : TRUE IN
                 /\ cs' = [cs EXCEPT ![x] = "closed", ![x2] = "open"]
                 /\ copy' = [copy EXCEPT ![x] = FALSE]
                 /\ owner' = [owner EXCEPT ![x2] = c]
                 /\ inbox' = [inbox EXCEPT ![x] = NoReq, ![x2] = inbox[x]]
                 /\ cur' = [cur EXCEPT ![c] = x2]
                 /\ tk' = [tk EXCEPT ![x] = NoTrack, ![x2] = Untracked]
            /\ UNCHANGED <<cst, queue>>
       ELSE /\ queue' = queue \cup {x}
            /\ tk' = [tk EXCEPT ![x] = [untracked |-> FALSE, across |-> tk[x].across]]
            /\ cst' = [cst EXCEPT ![c] = "sent"]
            /\ UNCHANGED <<cs, copy, owner, inbox, cur>>
    /\ UNCHANGED <<cache, calls, abandoned, outbox, kept, killed, ws, wc, held, runs,
                   pool, deaths, shutdown, cbWanted, falseCrash, wrongReply>>

CloseCallerEnd(x) ==
    IF copy[x] THEN cs
    ELSE [cs EXCEPT ![x] = IF cs[x] = "sclosed" THEN "closed" ELSE "cclosed"]

Finish(c) ==
    /\ calls' = [calls EXCEPT ![c] = @ + 1]
    /\ cst' = [cst EXCEPT ![c] = "idle"]
    /\ cbWanted' = IF c = "cb" THEN FALSE ELSE cbWanted

\* The reply arrives.
Receive(c) ==
    /\ cst[c] = "sent"
    /\ LET x == cur[c] IN
         /\ outbox[x] # NoReq
         /\ wrongReply' = (wrongReply \/ outbox[x] # Req(c))
         /\ outbox' = [outbox EXCEPT ![x] = NoReq]
         /\ IF kept[x]
            THEN /\ cache' = [cache EXCEPT ![c] = x]
                 /\ UNCHANGED cs
            ELSE /\ cache' = [cache EXCEPT ![c] = None]
                 /\ cs' = CloseCallerEnd(x)
    /\ Finish(c)
    /\ UNCHANGED <<cur, abandoned, owner, copy, inbox, kept, killed, queue, ws, wc, held,
                   runs, pool, deaths, shutdown, falseCrash, tk>>

\* End of file after a complete send: the callee is gone.
PeerGone(c) ==
    /\ cst[c] = "sent"
    /\ LET x == cur[c] IN
         /\ ~CalleeOpen(x)
         /\ outbox[x] = NoReq
         /\ falseCrash' = (falseCrash \/ ~killed[x])
         /\ IF Variant = "retry_after_complete_send" /\ pool = "up" /\ Fresh # {}
            THEN /\ cache' = [cache EXCEPT ![c] = None]
                 /\ LET x2 == CHOOSE y \in Fresh : TRUE IN
                      /\ cs' = [cs EXCEPT ![x] = "closed", ![x2] = "open"]
                      /\ owner' = [owner EXCEPT ![x2] = c]
                      /\ inbox' = [inbox EXCEPT ![x2] = Req(c)]
                      /\ queue' = queue \cup {x2}
                      /\ cur' = [cur EXCEPT ![c] = x2]
                 /\ UNCHANGED <<cst, calls, cbWanted>>
            ELSE /\ cache' = [cache EXCEPT ![c] = None]
                 /\ cs' = [cs EXCEPT ![x] = "closed"]
                 /\ Finish(c)
                 /\ UNCHANGED <<owner, inbox, queue, cur>>
    /\ UNCHANGED <<abandoned, copy, outbox, kept, killed, ws, wc, held, runs, pool,
                   deaths, shutdown, wrongReply, tk>>

\* The caller gives up on a call for a reason of its own, without its reply.
Abandon(c) ==
    /\ "abandon" \in Faults
    /\ c = "c"
    /\ cst[c] = "sent"
    /\ ~abandoned
    /\ abandoned' = TRUE
    /\ IF Variant = "keep_after_error"
       THEN /\ cache' = [cache EXCEPT ![c] = cur[c]]
            /\ UNCHANGED cs
       ELSE /\ cache' = [cache EXCEPT ![c] = None]
            /\ cs' = CloseCallerEnd(cur[c])
    /\ Finish(c)
    /\ UNCHANGED <<cur, owner, copy, inbox, outbox, kept, killed, queue, ws, wc, held,
                   runs, pool, deaths, shutdown, falseCrash, wrongReply, tk>>

\* The caller thread exits: its cache is closed.
Exit(c) ==
    /\ c # "cb"
    /\ cst[c] = "idle"
    /\ cst' = [cst EXCEPT ![c] = "exited"]
    /\ IF cache[c] # None
       THEN cs' = CloseCallerEnd(cache[c])
       ELSE UNCHANGED cs
    /\ cache' = [cache EXCEPT ![c] = None]
    /\ UNCHANGED <<cur, calls, abandoned, owner, copy, inbox, outbox, kept, killed,
                   queue, ws, wc, held, runs, pool, deaths, shutdown, cbWanted,
                   falseCrash, wrongReply, tk>>

\* The caller's process forks; the child is k.
\* Another thread may fork while c makes a socket: the child's copy of an
\* untracked socket is not closed by the fork handler.
Fork ==
    /\ "fork" \in Faults
    /\ cst["k"] = "unborn"
    /\ cst["c"] \in {"idle", "connecting"}
    /\ cst' = [cst EXCEPT !["k"] = "idle"]
    /\ cache' = IF Variant = "child_uses_inherited"
                THEN [cache EXCEPT !["k"] = cache["c"]]
                ELSE cache
    /\ copy' = [x \in Conns |-> copy[x] \/ tk[x].untracked
                  \/ (Variant = "child_keeps_inherited" /\ cache["c"] = x)]
    /\ tk' = [x \in Conns |-> IF tk[x].untracked THEN [tk[x] EXCEPT !.across = TRUE] ELSE tk[x]]
    /\ UNCHANGED <<cur, calls, abandoned, cs, owner, inbox, outbox, kept, killed,
                   queue, ws, wc, held, runs, pool, deaths, shutdown, cbWanted,
                   falseCrash, wrongReply>>

\* ---- the callee pool ----

Free == {w \in Workers : ws[w] = "idle" \/ (Variant = "finishing_counts_pinned" /\ ws[w] = "pinned")}

\* The accept loop starts a worker for a queued connection no worker can take.
Spawn ==
    /\ pool = "up"
    /\ ~shutdown
    /\ Cardinality(queue) > Cardinality(Free)
    /\ \E w \in Workers :
         /\ ws[w] = "absent"
         /\ ws' = [ws EXCEPT ![w] = "idle"]
    /\ UNCHANGED <<cst, cache, cur, calls, abandoned, cs, owner, copy, inbox,
                   outbox, kept, killed, queue, wc, held, runs, pool, deaths, shutdown,
                   cbWanted, falseCrash, wrongReply, tk>>

Take(w) ==
    /\ pool = "up"
    /\ ws[w] = "idle"
    /\ queue # {}
    /\ LET x == CHOOSE y \in queue : TRUE IN
         /\ queue' = queue \ {x}
         /\ wc' = [wc EXCEPT ![w] = x]
         /\ ws' = [ws EXCEPT ![w] = "serving"]
    /\ UNCHANGED <<held, cst, cache, cur, calls, abandoned, cs, owner, copy, inbox,
                   outbox, kept, killed, runs, pool, deaths, shutdown, cbWanted,
                   falseCrash, wrongReply, tk>>

\* A serving or pinned worker reads its connection's request and runs it.
\* The first call from c calls back: the worker blocks until cb's call ends.
Run(w) ==
    /\ pool = "up"
    /\ ws[w] \in {"serving", "pinned"}
    /\ LET x == wc[w] IN
         /\ inbox[x] # NoReq
         /\ runs' = [runs EXCEPT ![inbox[x]] = @ + 1]
         /\ inbox' = [inbox EXCEPT ![x] = NoReq]
         /\ IF inbox[x].caller = "c" /\ ~cbWanted /\ calls["cb"] = 0
            THEN /\ ws' = [ws EXCEPT ![w] = "blocked"]
                 /\ held' = [held EXCEPT ![w] = inbox[x]]
                 /\ cbWanted' = TRUE
                 /\ UNCHANGED <<outbox, kept>>
            ELSE /\ ws' = [ws EXCEPT ![w] = "pinned"]
                 /\ outbox' = [outbox EXCEPT ![x] = inbox[x]]
                 /\ kept' = [kept EXCEPT ![x] = TRUE]
                 /\ UNCHANGED <<held, cbWanted>>
    /\ UNCHANGED <<cst, cache, cur, calls, abandoned, cs, owner, copy, killed,
                   queue, wc, pool, deaths, shutdown, falseCrash, wrongReply, tk>>

\* The callback has returned: the blocked worker replies.
Resume(w) ==
    /\ pool = "up"
    /\ ws[w] = "blocked"
    /\ ~cbWanted
    /\ outbox' = [outbox EXCEPT ![wc[w]] = held[w]]
    /\ kept' = [kept EXCEPT ![wc[w]] = TRUE]
    /\ ws' = [ws EXCEPT ![w] = "pinned"]
    /\ held' = [held EXCEPT ![w] = NoReq]
    /\ UNCHANGED <<cst, cache, cur, calls, abandoned, cs, owner, copy, inbox,
                   killed, queue, wc, runs, pool, deaths, shutdown, cbWanted,
                   falseCrash, wrongReply, tk>>

\* A pinned worker sees end of file: it closes its end and is free again.
Unpin(w) ==
    /\ ws[w] = "pinned"
    /\ cs[wc[w]] = "cclosed"
    /\ inbox[wc[w]] = NoReq
    /\ cs' = [cs EXCEPT ![wc[w]] = "closed"]
    /\ ws' = [ws EXCEPT ![w] = "idle"]
    /\ wc' = [wc EXCEPT ![w] = None]
    /\ UNCHANGED <<cst, cache, cur, calls, abandoned, owner, copy, inbox, outbox,
                   kept, killed, queue, held, runs, pool, deaths, shutdown,
                   cbWanted, falseCrash, wrongReply, tk>>


CloseCalleeEnd(x) == IF cs[x] = "cclosed" THEN "closed" ELSE "sclosed"

\* Shutdown, with no call in flight anywhere.
Shutdown ==
    /\ ~shutdown
    /\ \A c \in Callers : cst[c] \notin {"sent", "connecting"}
    /\ \A w \in Workers : ws[w] \notin {"serving", "blocked"}
    /\ queue = {}
    /\ ~cbWanted
    /\ shutdown' = TRUE
    /\ UNCHANGED <<cst, cache, cur, calls, abandoned, cs, owner, copy, inbox,
                   outbox, kept, killed, queue, ws, wc, held, runs, pool, deaths,
                   cbWanted, falseCrash, wrongReply, tk>>

\* A worker that is idle, or pinned and woken by shutdown, exits.
Leave(w) ==
    /\ shutdown
    /\ \/ /\ ws[w] = "idle"
          /\ UNCHANGED cs
       \/ /\ ws[w] = "pinned"
          /\ Variant # "join_pinned_without_shutdown_wake"
          /\ cs' = [cs EXCEPT ![wc[w]] = CloseCalleeEnd(wc[w])]
    /\ ws' = [ws EXCEPT ![w] = "gone"]
    /\ UNCHANGED <<cst, cache, cur, calls, abandoned, owner, copy, inbox, outbox,
                   kept, killed, queue, wc, held, runs, pool, deaths, shutdown,
                   cbWanted, falseCrash, wrongReply, tk>>

\* The pool dies: every callee end closes with it.
Die ==
    /\ "death" \in Faults
    /\ pool = "up"
    /\ deaths = 0
    /\ ~shutdown
    /\ pool' = "dead"
    /\ deaths' = 1
    /\ ws' = [w \in Workers |-> "absent"]
    /\ wc' = [w \in Workers |-> None]
    /\ held' = [w \in Workers |-> NoReq]
    /\ queue' = {}
    /\ cs' = [x \in Conns |-> IF CalleeOpen(x) /\ ~tk[x].untracked THEN CloseCalleeEnd(x) ELSE cs[x]]
    /\ killed' = [x \in Conns |-> killed[x] \/ (CalleeOpen(x) /\ ~tk[x].untracked)]
    /\ UNCHANGED <<cst, cache, cur, calls, abandoned, owner, copy, inbox, outbox,
                   kept, runs, shutdown, cbWanted, falseCrash, wrongReply, tk>>

Restart ==
    /\ pool = "dead"
    /\ pool' = "up"
    /\ ws' = [ws EXCEPT !["w1"] = "idle"]
    /\ UNCHANGED <<cst, cache, cur, calls, abandoned, cs, owner, copy, inbox,
                   outbox, kept, killed, queue, wc, held, runs, deaths, shutdown,
                   cbWanted, falseCrash, wrongReply, tk>>

\* A connection closed at both ends and held by no live worker or call
\* frees its id, as the kernel reuses a descriptor number.
Recycle(x) ==
    /\ cs[x] = "closed"
    /\ \A w \in Workers : ws[w] \in {"absent", "gone"} \/ wc[w] # x
    /\ \A c \in Callers : ~(cst[c] = "sent" /\ cur[c] = x) /\ cache[c] # x
    /\ cs' = [cs EXCEPT ![x] = "unused"]
    /\ inbox' = [inbox EXCEPT ![x] = NoReq]
    /\ outbox' = [outbox EXCEPT ![x] = NoReq]
    /\ kept' = [kept EXCEPT ![x] = FALSE]
    /\ killed' = [killed EXCEPT ![x] = FALSE]
    /\ copy' = [copy EXCEPT ![x] = FALSE]
    /\ tk' = [tk EXCEPT ![x] = NoTrack]
    /\ wc' = [w \in Workers |-> IF wc[w] = x THEN None ELSE wc[w]]
    /\ UNCHANGED <<cst, cache, cur, calls, abandoned, owner, queue, ws, held, runs,
                   pool, deaths, shutdown, cbWanted, falseCrash, wrongReply>>

Next ==
    \/ \E c \in Callers : Call(c) \/ Track(c) \/ Receive(c) \/ PeerGone(c) \/ Abandon(c) \/ Exit(c)
    \/ Fork
    \/ Spawn
    \/ \E w \in Workers : Take(w) \/ Run(w) \/ Resume(w) \/ Unpin(w) \/ Leave(w)
    \/ Shutdown
    \/ Die
    \/ Restart
    \/ \E x \in Conns : Recycle(x)
    \/ UNCHANGED vars

Spec ==
    /\ Init
    /\ [][Next]_vars
    /\ \A c \in Callers : WF_vars(Receive(c)) /\ WF_vars(PeerGone(c)) /\ WF_vars(Track(c))
    /\ WF_vars(Call("cb"))  \* the call back is part of the call that waits on it
    /\ WF_vars(Spawn)
    /\ WF_vars(Restart)
    /\ \A x \in Conns : WF_vars(Recycle(x))
    /\ \A w \in Workers : WF_vars(Take(w)) /\ WF_vars(Run(w)) /\ WF_vars(Resume(w))
                         /\ WF_vars(Unpin(w)) /\ WF_vars(Leave(w))

\* ---- properties ----

AtMostOnce == \A r \in ReqIds : runs[r] <= 1
NoWrongReply == ~wrongReply
NoFalseCrash == ~falseCrash
SingleUse == \A x \in Conns :
    Cardinality({c \in Callers : cache[c] = x \/ (cst[c] = "sent" /\ cur[c] = x)}) <= 1

\* Every call ends, in a reply or in its callee's death.
NoStrandedCall == \A c \in Callers : cst[c] = "sent" ~> cst[c] # "sent"

\* A worker pinned to a connection whose caller has exited is freed.
ExitFreesWorker == \A w \in Workers :
    (ws[w] = "pinned" /\ cst[owner[wc[w]]] = "exited") ~> ws[w] # "pinned"

ShutdownTerminates == shutdown ~> (\A w \in Workers : ws[w] \in {"absent", "gone"})
================================================================================
