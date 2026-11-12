------------------------------- MODULE KeptConn -------------------------------
(* Kept caller connections into a fork pool (Python, R). A caller thread  *)
(* caches one connection per callee and reuses it when the last reply     *)
(* said the callee keeps it (NET-7). A worker that replied with the keep  *)
(* bit holds that connection while it is idle and polls it beside the     *)
(* listening socket; it is never pinned and never counts busy. It serves  *)
(* a request on its held connection, or accepts a new one; before it      *)
(* serves a new one it revokes what it holds: it appends a not-run notice *)
(* and closes its end without reading again, so a request sent on the     *)
(* revoked connection was never run and its caller retries it once on a  *)
(* fresh connection. A retiring worker revokes the same way. A request    *)
(* sent on a connection whose callee end is closed delivered nothing and  *)
(* is retried (NET-7); end of file after a complete send with no reply    *)
(* is the pool's death. t1's call calls back into the caller's pool, whose *)
(* idle thread (t2, or t3 when present) calls this pool before t1's call *)
(* can return.                                                             *)
(* The coordinator forks a worker when every live worker waits on another *)
(* pool (NET-9).                                                           *)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS Variant, Faults, Threads, Workers, NConns, Calls2
\* Threads: {"t1", "t2"} or {"t1", "t2", "t3"}. t1 makes one call, which
\*   calls back; t2 makes Calls2 calls; t3 only takes the callback.
\* Workers: worker ids, "w1" among them; w1 starts the pool.
\* NConns: connection ids, recycled once closed at both ends.
\* Faults: which of "death", "retire" and "replyfail" may happen in a run;
\*   each at most once. "replyfail": a reply is not sent whole, and the call
\*   fails.
\* Variant:
\* "design": the design.
\* "serve_while_holding": a worker accepts a new connection and keeps the one
\*   it holds; a request on the held one waits behind the new call.
\* "revoke_without_notice": a worker revokes its held connection by closing
\*   it without a notice.
\* "retire_silently": a retiring worker closes its held connection without a
\*   notice.
\* "notice_keeps_reading": a revoking worker sends the notice but keeps
\*   reading the connection.
\* "hold_on_asked": a worker holds a connection whose request asked to keep,
\*   even when its reply was not sent.
\* "ignore_eof": a worker never lets go of a held connection its caller
\*   closed.

Own(t) == CASE t = "t1" -> 1 [] t = "t2" -> Calls2 [] OTHER -> 0
Conns == 1..NConns
Helpers == Threads \ {"t1"}
None == 0
NoReq == [who |-> "none", n |-> 0]
NotRun == [who |-> "notrun", n |-> 0]
ReqIds == [who : Threads, n : 1..(Calls2 + 1)]
CallsBack(r) == r = [who |-> "t1", n |-> 1]

VARIABLES
    ts,         \* thread -> "idle" | "waiting" | "exited"
    cache,      \* thread -> cached connection or None
    cur,        \* thread -> connection of the call in flight
    curReq,     \* thread -> request in flight
    isCb,       \* thread -> the call in flight is the callback's
    calls,      \* thread -> calls ended
    own,        \* thread -> own calls ended
    cs,         \* conn -> "unused" | "open" | "cclosed" | "wclosed" | "closed"
    inbox,      \* conn -> request sent and not read, or NoReq
    outbox,     \* conn -> sequence of replies and notices not yet read
    kept,       \* conn -> a reply on it carried the keep bit
    killed,     \* conn -> its worker end closed with the pool's death or a failed reply
    backlog,    \* connections queued on the listening socket
    ws,         \* worker -> "absent" | "idle" | "computing" | "blocked" | "gone"
    held,       \* worker -> connections it holds for their next request
    srv,        \* worker -> connection it serves, or None
    req,        \* worker -> request it serves
    runs,       \* request -> times run
    poolUp,
    deaths,
    retired,
    failed,
    cbState,    \* "none" | "wanted" | "taken" | "sent" | "done"
    cbBy,       \* thread that took the callback
    falseCrash,
    wrongReply,
    notRunLie

vars == <<ts, cache, cur, curReq, isCb, calls, own, cs, inbox, outbox, kept,
          killed, backlog, ws, held, srv, req, runs, poolUp, deaths, retired, failed,
          cbState, cbBy, falseCrash, wrongReply, notRunLie>>

callerVars == <<ts, cache, cur, curReq, isCb, calls, own>>
connVars == <<cs, inbox, outbox, kept, killed, backlog>>
workerVars == <<ws, held, srv, req>>
otherVars == <<runs, poolUp, deaths, retired, failed, cbState, cbBy, falseCrash, wrongReply, notRunLie>>

Init ==
    /\ ts = [t \in Threads |-> "idle"]
    /\ cache = [t \in Threads |-> None]
    /\ cur = [t \in Threads |-> None]
    /\ curReq = [t \in Threads |-> NoReq]
    /\ isCb = [t \in Threads |-> FALSE]
    /\ calls = [t \in Threads |-> 0]
    /\ own = [t \in Threads |-> 0]
    /\ cs = [x \in Conns |-> "unused"]
    /\ inbox = [x \in Conns |-> NoReq]
    /\ outbox = [x \in Conns |-> <<>>]
    /\ kept = [x \in Conns |-> FALSE]
    /\ killed = [x \in Conns |-> FALSE]
    /\ backlog = {}
    /\ ws = [w \in Workers |-> IF w = "w1" THEN "idle" ELSE "absent"]
    /\ held = [w \in Workers |-> {}]
    /\ srv = [w \in Workers |-> None]
    /\ req = [w \in Workers |-> NoReq]
    /\ runs = [r \in ReqIds |-> 0]
    /\ poolUp = TRUE
    /\ deaths = 0
    /\ retired = 0
    /\ failed = 0
    /\ cbState = "none"
    /\ cbBy = "none"
    /\ falseCrash = FALSE
    /\ wrongReply = FALSE
    /\ notRunLie = FALSE

WorkerOpen(x) == cs[x] \in {"open", "cclosed"}
CloseCaller(c, x) == IF c[x] = "wclosed" THEN "closed" ELSE "cclosed"
CloseWorker(c, x) == IF c[x] = "cclosed" THEN "closed" ELSE "wclosed"
Fresh == {x \in Conns : cs[x] = "unused"}
Live == {w \in Workers : ws[w] \in {"idle", "computing", "blocked"}}
Blocked == {w \in Workers : ws[w] = "blocked"}

\* ---- callers ----

\* A fresh connection carrying r, queued on the listening socket; the caller
\* end of `drop` (a dead or revoked connection) closes with it.
Connect(t, r, drop) ==
    /\ poolUp
    /\ Fresh # {}
    /\ LET x == CHOOSE y \in Fresh : TRUE
           c1 == IF drop = None THEN cs ELSE [cs EXCEPT ![drop] = CloseCaller(cs, drop)]
       IN /\ cs' = [c1 EXCEPT ![x] = "open"]
          /\ inbox' = [inbox EXCEPT ![x] = r]
          /\ backlog' = backlog \cup {x}
          /\ cur' = [cur EXCEPT ![t] = x]

\* A send on a cached connection whose callee end is open is delivered; one
\* whose callee end is closed fails having delivered nothing (EPIPE).
StartCall(t, r, cb) ==
    /\ curReq' = [curReq EXCEPT ![t] = r]
    /\ isCb' = [isCb EXCEPT ![t] = cb]
    /\ ts' = [ts EXCEPT ![t] = "waiting"]
    /\ IF cache[t] # None /\ WorkerOpen(cache[t])
       THEN /\ inbox' = [inbox EXCEPT ![cache[t]] = r]
            /\ cur' = [cur EXCEPT ![t] = cache[t]]
            /\ UNCHANGED <<cs, backlog, cache>>
       ELSE /\ cache' = [cache EXCEPT ![t] = None]
            /\ Connect(t, r, cache[t])

CallOwn(t) ==
    /\ ts[t] = "idle"
    /\ own[t] < Own(t)
    /\ ~(cbBy = t /\ cbState = "taken")
    /\ StartCall(t, [who |-> t, n |-> calls[t] + 1], FALSE)
    /\ UNCHANGED <<calls, own, outbox, kept, killed>>
    /\ UNCHANGED workerVars
    /\ UNCHANGED otherVars

\* An idle thread of the caller's pool takes the callback.
TakeCallback(t) ==
    /\ t \in Helpers
    /\ cbState = "wanted"
    /\ ts[t] = "idle"
    /\ cbState' = "taken"
    /\ cbBy' = t
    /\ UNCHANGED callerVars
    /\ UNCHANGED connVars
    /\ UNCHANGED workerVars
    /\ UNCHANGED <<runs, poolUp, deaths, retired, failed, falseCrash, wrongReply, notRunLie>>

CallCb(t) ==
    /\ ts[t] = "idle"
    /\ cbState = "taken"
    /\ cbBy = t
    /\ cbState' = "sent"
    /\ StartCall(t, [who |-> t, n |-> calls[t] + 1], TRUE)
    /\ UNCHANGED <<calls, own, outbox, kept, killed>>
    /\ UNCHANGED workerVars
    /\ UNCHANGED <<runs, poolUp, deaths, retired, failed, cbBy, falseCrash, wrongReply, notRunLie>>

Finish(t) ==
    /\ calls' = [calls EXCEPT ![t] = @ + 1]
    /\ own' = IF isCb[t] THEN own ELSE [own EXCEPT ![t] = @ + 1]
    /\ ts' = [ts EXCEPT ![t] = "idle"]
    /\ cbState' = IF isCb[t] THEN "done" ELSE cbState

\* The next message on the call's connection: its reply, or a notice that
\* the request was not run, which is retried on a fresh connection.
Receive(t) ==
    /\ ts[t] = "waiting"
    /\ LET x == cur[t] IN
         /\ outbox[x] # <<>>
         /\ outbox' = [outbox EXCEPT ![x] = Tail(@)]
         /\ IF Head(outbox[x]) = NotRun
            THEN /\ notRunLie' = (notRunLie \/ runs[curReq[t]] > 0)
                 /\ cache' = [cache EXCEPT ![t] = None]
                 /\ Connect(t, curReq[t], x)
                 /\ UNCHANGED <<ts, calls, own, cbState, wrongReply>>
            ELSE /\ wrongReply' = (wrongReply \/ Head(outbox[x]) # curReq[t])
                 /\ IF kept[x]
                    THEN /\ cache' = [cache EXCEPT ![t] = x]
                         /\ UNCHANGED cs
                    ELSE /\ cache' = [cache EXCEPT ![t] = None]
                         /\ cs' = [cs EXCEPT ![x] = CloseCaller(cs, x)]
                 /\ Finish(t)
                 /\ UNCHANGED <<notRunLie, cur, inbox, backlog>>
    /\ UNCHANGED <<curReq, isCb, kept, killed>>
    /\ UNCHANGED workerVars
    /\ UNCHANGED <<runs, poolUp, deaths, retired, failed, cbBy, falseCrash>>

\* End of file after a complete send, with nothing to read: the callee is
\* gone, and the call fails without a retry.
PeerGone(t) ==
    /\ ts[t] = "waiting"
    /\ LET x == cur[t] IN
         /\ ~WorkerOpen(x)
         /\ outbox[x] = <<>>
         /\ falseCrash' = (falseCrash \/ ~killed[x])
         /\ cs' = [cs EXCEPT ![x] = "closed"]
         /\ cache' = [cache EXCEPT ![t] = None]
    /\ Finish(t)
    /\ UNCHANGED <<cur, curReq, isCb, inbox, outbox, kept, killed, backlog>>
    /\ UNCHANGED workerVars
    /\ UNCHANGED <<runs, poolUp, deaths, retired, failed, cbBy, wrongReply, notRunLie>>

\* t2 ends once its calls are made: its cache closes. Without t3 it stays
\* until the callback is done, since nobody else can take it.
Exit(t) ==
    /\ t = "t2"
    /\ ts[t] = "idle"
    /\ own[t] = Own(t)
    /\ ~(cbBy = t /\ cbState = "taken")
    /\ "t3" \notin Threads => cbState = "done"
    /\ ts' = [ts EXCEPT ![t] = "exited"]
    /\ IF cache[t] # None
       THEN cs' = [cs EXCEPT ![cache[t]] = CloseCaller(cs, cache[t])]
       ELSE UNCHANGED cs
    /\ cache' = [cache EXCEPT ![t] = None]
    /\ UNCHANGED <<cur, curReq, isCb, calls, own, inbox, outbox, kept, killed, backlog>>
    /\ UNCHANGED workerVars
    /\ UNCHANGED otherVars

\* ---- the pool ----

Spawn ==
    /\ poolUp
    /\ Cardinality(Blocked) >= Cardinality(Live)
    /\ \E w \in Workers :
         /\ ws[w] \in {"absent", "gone"}
         /\ ws' = [ws EXCEPT ![w] = "idle"]
         /\ held' = [held EXCEPT ![w] = {}]
         /\ srv' = [srv EXCEPT ![w] = None]
         /\ req' = [req EXCEPT ![w] = NoReq]
    /\ UNCHANGED callerVars
    /\ UNCHANGED connVars
    /\ UNCHANGED otherVars

\* An idle worker reads the next request on a connection it holds.
ServeKept(w) ==
    /\ ws[w] = "idle"
    /\ \E x \in held[w] :
         /\ WorkerOpen(x)
         /\ inbox[x] # NoReq
         /\ runs' = [runs EXCEPT ![inbox[x]] = @ + 1]
         /\ req' = [req EXCEPT ![w] = inbox[x]]
         /\ inbox' = [inbox EXCEPT ![x] = NoReq]
         /\ srv' = [srv EXCEPT ![w] = x]
         /\ held' = [held EXCEPT ![w] = @ \ {x}]
         /\ ws' = [ws EXCEPT ![w] = "computing"]
    /\ UNCHANGED callerVars
    /\ UNCHANGED <<cs, outbox, kept, killed, backlog>>
    /\ UNCHANGED <<poolUp, deaths, retired, failed, cbState, cbBy, falseCrash, wrongReply, notRunLie>>

\* An idle worker accepts a queued connection, revokes what it holds, and
\* reads the new connection's request.
Accept(w) ==
    /\ ws[w] = "idle"
    /\ \E x \in backlog :
         LET N == IF Variant \in {"serve_while_holding", "revoke_without_notice"}
                  THEN {} ELSE held[w]
             S == IF Variant \in {"serve_while_holding", "notice_keeps_reading"}
                  THEN {} ELSE held[w]
         IN /\ backlog' = backlog \ {x}
            /\ cs' = [y \in Conns |-> IF y \in S THEN CloseWorker(cs, y) ELSE cs[y]]
            /\ inbox' = [y \in Conns |-> IF y \in S \/ y = x THEN NoReq ELSE inbox[y]]
            /\ outbox' = [y \in Conns |-> IF y \in N
                                          THEN Append(outbox[y], NotRun) ELSE outbox[y]]
            /\ runs' = [runs EXCEPT ![inbox[x]] = @ + 1]
            /\ req' = [req EXCEPT ![w] = inbox[x]]
            /\ srv' = [srv EXCEPT ![w] = x]
            /\ held' = [held EXCEPT ![w] = @ \ S]
            /\ ws' = [ws EXCEPT ![w] = "computing"]
    /\ UNCHANGED callerVars
    /\ UNCHANGED <<kept, killed>>
    /\ UNCHANGED <<poolUp, deaths, retired, failed, cbState, cbBy, falseCrash, wrongReply, notRunLie>>

\* t1's call calls back into the caller's pool and waits for that call.
CallBack(w) ==
    /\ ws[w] = "computing"
    /\ CallsBack(req[w])
    /\ cbState = "none"
    /\ ws' = [ws EXCEPT ![w] = "blocked"]
    /\ cbState' = "wanted"
    /\ UNCHANGED callerVars
    /\ UNCHANGED connVars
    /\ UNCHANGED <<held, srv, req>>
    /\ UNCHANGED <<runs, poolUp, deaths, retired, failed, cbBy, falseCrash, wrongReply, notRunLie>>

Resume(w) ==
    /\ ws[w] = "blocked"
    /\ cbState = "done"
    /\ ws' = [ws EXCEPT ![w] = "computing"]
    /\ UNCHANGED callerVars
    /\ UNCHANGED connVars
    /\ UNCHANGED <<held, srv, req>>
    /\ UNCHANGED otherVars

\* The reply carries the keep bit; the worker holds the connection.
Reply(w) ==
    /\ ws[w] = "computing"
    /\ CallsBack(req[w]) => cbState = "done"
    /\ LET x == srv[w] IN
         /\ outbox' = [outbox EXCEPT ![x] = Append(@, req[w])]
         /\ kept' = [kept EXCEPT ![x] = TRUE]
         /\ held' = [held EXCEPT ![w] = @ \cup {x}]
    /\ srv' = [srv EXCEPT ![w] = None]
    /\ ws' = [ws EXCEPT ![w] = "idle"]
    /\ UNCHANGED callerVars
    /\ UNCHANGED <<cs, inbox, killed, backlog, req>>
    /\ UNCHANGED otherVars

\* The reply is not sent whole: the connection closes and the call fails.
ReplyFail(w) ==
    /\ "replyfail" \in Faults
    /\ failed = 0
    /\ ws[w] = "computing"
    /\ CallsBack(req[w]) => cbState = "done"
    /\ LET x == srv[w] IN
         IF Variant = "hold_on_asked"
         THEN /\ held' = [held EXCEPT ![w] = @ \cup {x}]
              /\ UNCHANGED <<cs, killed>>
         ELSE /\ cs' = [cs EXCEPT ![x] = CloseWorker(cs, x)]
              /\ killed' = [killed EXCEPT ![x] = TRUE]
              /\ UNCHANGED held
    /\ srv' = [srv EXCEPT ![w] = None]
    /\ ws' = [ws EXCEPT ![w] = "idle"]
    /\ failed' = 1
    /\ UNCHANGED callerVars
    /\ UNCHANGED <<inbox, outbox, kept, backlog, req>>
    /\ UNCHANGED <<runs, poolUp, deaths, retired, cbState, cbBy, falseCrash, wrongReply, notRunLie>>

\* A held connection whose caller closed it is closed and let go.
Eof(w) ==
    /\ Variant # "ignore_eof"
    /\ ws[w] = "idle"
    /\ \E x \in held[w] :
         /\ cs[x] = "cclosed"
         /\ inbox[x] = NoReq
         /\ cs' = [cs EXCEPT ![x] = "closed"]
         /\ held' = [held EXCEPT ![w] = @ \ {x}]
    /\ UNCHANGED callerVars
    /\ UNCHANGED <<inbox, outbox, kept, killed, backlog>>
    /\ UNCHANGED <<ws, srv, req>>
    /\ UNCHANGED otherVars

\* A surplus idle worker retires with nothing queued, revoking what it holds.
Retire(w) ==
    /\ "retire" \in Faults
    /\ retired = 0
    /\ ws[w] = "idle"
    /\ Cardinality(Live) > 1
    /\ backlog = {}
    /\ LET S == held[w]
           notice == Variant # "retire_silently"
       IN /\ cs' = [y \in Conns |-> IF y \in S THEN CloseWorker(cs, y) ELSE cs[y]]
          /\ inbox' = [y \in Conns |-> IF y \in S THEN NoReq ELSE inbox[y]]
          /\ outbox' = [y \in Conns |-> IF y \in S /\ notice
                                        THEN Append(outbox[y], NotRun) ELSE outbox[y]]
    /\ held' = [held EXCEPT ![w] = {}]
    /\ ws' = [ws EXCEPT ![w] = "gone"]
    /\ retired' = 1
    /\ UNCHANGED callerVars
    /\ UNCHANGED <<kept, killed, backlog>>
    /\ UNCHANGED <<srv, req>>
    /\ UNCHANGED <<runs, poolUp, deaths, failed, cbState, cbBy, falseCrash, wrongReply, notRunLie>>

\* A worker crashes, which ends the pool (SHM-8): every callee end closes.
Die ==
    /\ "death" \in Faults
    /\ poolUp
    /\ deaths = 0
    /\ poolUp' = FALSE
    /\ deaths' = 1
    /\ ws' = [w \in Workers |-> "absent"]
    /\ held' = [w \in Workers |-> {}]
    /\ srv' = [w \in Workers |-> None]
    /\ req' = [w \in Workers |-> NoReq]
    /\ backlog' = {}
    /\ cs' = [x \in Conns |-> IF WorkerOpen(x) THEN CloseWorker(cs, x) ELSE cs[x]]
    /\ killed' = [x \in Conns |-> killed[x] \/ WorkerOpen(x)]
    /\ inbox' = [x \in Conns |-> NoReq]
    /\ UNCHANGED callerVars
    /\ UNCHANGED <<outbox, kept>>
    /\ UNCHANGED <<runs, retired, failed, cbState, cbBy, falseCrash, wrongReply, notRunLie>>

Restart ==
    /\ ~poolUp
    /\ poolUp' = TRUE
    /\ UNCHANGED callerVars
    /\ UNCHANGED connVars
    /\ UNCHANGED workerVars
    /\ UNCHANGED <<runs, deaths, retired, failed, cbState, cbBy, falseCrash, wrongReply, notRunLie>>

\* A connection closed at both ends and referenced by nobody frees its id.
Recycle(x) ==
    /\ cs[x] = "closed"
    /\ x \notin backlog
    /\ \A w \in Workers : x \notin held[w] /\ srv[w] # x
    /\ \A t \in Threads : cache[t] # x /\ ~(ts[t] = "waiting" /\ cur[t] = x)
    /\ cs' = [cs EXCEPT ![x] = "unused"]
    /\ inbox' = [inbox EXCEPT ![x] = NoReq]
    /\ outbox' = [outbox EXCEPT ![x] = <<>>]
    /\ kept' = [kept EXCEPT ![x] = FALSE]
    /\ killed' = [killed EXCEPT ![x] = FALSE]
    /\ UNCHANGED callerVars
    /\ UNCHANGED backlog
    /\ UNCHANGED workerVars
    /\ UNCHANGED otherVars

Next ==
    \/ \E t \in Threads :
         CallOwn(t) \/ TakeCallback(t) \/ CallCb(t) \/ Receive(t) \/ PeerGone(t) \/ Exit(t)
    \/ Spawn
    \/ \E w \in Workers :
         ServeKept(w) \/ Accept(w) \/ CallBack(w) \/ Resume(w) \/ Reply(w) \/ ReplyFail(w)
         \/ Eof(w) \/ Retire(w)
    \/ Die
    \/ Restart
    \/ \E x \in Conns : Recycle(x)
    \/ UNCHANGED vars

Spec ==
    /\ Init
    /\ [][Next]_vars
    /\ \A t \in Threads : WF_vars(Receive(t)) /\ WF_vars(PeerGone(t)) /\ WF_vars(CallCb(t))
    /\ WF_vars(\E t \in Helpers : TakeCallback(t))
    /\ WF_vars(Spawn)
    /\ WF_vars(Restart)
    /\ \A x \in Conns : WF_vars(Recycle(x))
    /\ \A w \in Workers : WF_vars(ServeKept(w)) /\ WF_vars(Accept(w)) /\ WF_vars(CallBack(w))
                         /\ WF_vars(Resume(w)) /\ WF_vars(Reply(w)) /\ WF_vars(Eof(w))

\* ---- properties ----

AtMostOnce == \A r \in ReqIds : runs[r] <= 1
NoWrongReply == ~wrongReply
\* A call fails only when the pool died or its reply was not sent.
NoFalseCrash == ~falseCrash
\* A request a notice says was not run was not run.
NotRunIsTrue == ~notRunLie
\* A worker holds at most one connection, and none while it serves.
HoldsOnlyWhenIdle == \A w \in Workers :
    /\ Cardinality(held[w]) <= 1
    /\ ws[w] # "idle" => held[w] = {}

\* Every call ends, in a reply or a failure.
NoStrandedCall == \A t \in Threads : ts[t] = "waiting" ~> ts[t] # "waiting"

\* A held connection its caller closed is let go.
EofLetsGo == \A w \in Workers, x \in Conns :
    (x \in held[w] /\ cs[x] = "cclosed") ~> (x \notin held[w])
================================================================================
