---------------------------- MODULE DaemonShutdown ----------------------------
(* Daemon shutdown (model/daemon.md DAEMON-6). A worker may be inside a     *)
(* call to a pool, which a healthy pool answers and a wedged one answers    *)
(* only once killed, inside a wait that stopping the pools does not end (a *)
(* client, an evaluation in the daemon itself), or waiting on a child      *)
(* process group it started (a forked compiler), which ends when the group *)
(* is killed. A shutdown signal may arrive at any time, including during a *)
(* pool crash recovery, which runs on the main thread. Shutdown waits a    *)
(* grace period, stops the pools and every child group, waits again, and   *)
(* then exits. Exit removes the names of shared memory but never unmaps it *)
(* (DAEMON-5): threads no request accounts for, such as compressors and    *)
(* user threads, may still read it, and the kernel unmaps only once every  *)
(* thread has stopped. No child group outlives the daemon.                 *)
EXTENDS Naturals

CONSTANT Variant
\* "bounded": the design.
\* "join_first": join the workers with no bound before stopping the pools.
\* "unmap_on_give_up": after the second wait, unmap even if workers remain.
\* "unmap_at_exit": exit unmaps shared memory once every worker returned.
\* "recovery_unmaps": a recovery whose wait for requests runs out unmaps.
\* "children_survive": stopping the pools leaves child groups running.
\* "panic_continues": a worker that panics is replaced and the daemon
\* serves on.

Workers == {"w1", "w2"}
Idle == "idle"
PoolCall == "pool"
OtherWait == "other"
ChildWait == "child"

(* --algorithm DaemonShutdown
variables
    poolWedged \in BOOLEAN,
    otherEnds \in BOOLEAN,
    state = [w \in Workers |-> Idle],
    child = [w \in Workers |-> FALSE],
    joined = {},
    poolsKilled = FALSE,
    recovering = FALSE,
    unmapUnderAWorker = FALSE,
    \* A thread no request accounts for (a compressor, a user thread) that
    \* may be reading shared memory when the daemon exits.
    background \in BOOLEAN,
    exited = FALSE,
    shutdown = FALSE,
    panicked = FALSE,
    failed = FALSE,
    servingOn = FALSE;

define
    NoUnmapUnderARunningWorker == ~unmapUnderAWorker
    NoChildOutlivesTheDaemon == exited => \A w \in Workers : ~child[w]
    ShutdownFinishes == shutdown ~> exited
    APanicEndsTheDaemonAsFailed == panicked ~> (exited /\ failed)
end define;

macro unmap() begin
  if \E w \in Workers : state[w] /= Idle then
    unmapUnderAWorker := TRUE;
  end if;
end macro;

macro exit_unmap() begin
  if background \/ \E w \in Workers : state[w] /= Idle then
    unmapUnderAWorker := TRUE;
  end if;
end macro;

macro stop_pools() begin
  poolsKilled := TRUE;
  if Variant /= "children_survive" then
    child := [w \in Workers |-> FALSE];
  end if;
end macro;

fair process Worker \in Workers
begin
  Pick:
    \* Admission and entry are one step, under the request gate's lock.
    if ~recovering /\ ~shutdown then
      either
        state[self] := PoolCall;
      or
        state[self] := OtherWait;
      or
        state[self] := ChildWait;
        child[self] := TRUE;
      or
        \* A runtime bug: the request is answered and the worker leaves.
        panicked := TRUE;
        if Variant /= "panic_continues" then
          shutdown := TRUE;
        end if;
      or
        skip;
      end either;
    end if;
  Wait:
    if state[self] = PoolCall then
      await exited \/ servingOn \/ ~poolWedged \/ poolsKilled;
    elsif state[self] = OtherWait then
      await exited \/ servingOn \/ otherEnds;
    elsif state[self] = ChildWait then
      await exited \/ servingOn \/ otherEnds \/ ~child[self];
    end if;
    if exited \/ servingOn then
      goto Done;
    end if;
  Return:
    state[self] := Idle;
    child[self] := FALSE;
  Finish:
    joined := joined \union {self};
end process;

fair process Signal = "signal"
begin
  Request:
    either
      shutdown := TRUE;
    or
      skip;
    end either;
end process;

fair process Main = "main"
variables gaveUp = FALSE;
begin
  Recover:
    either
      goto Serve;
    or
      poolsKilled := TRUE;
      recovering := TRUE;
    end either;
  Drain:
    either
      await \A w \in Workers : state[w] = Idle;
      if shutdown then
        recovering := FALSE;
        goto Serve;
      end if;
    or
      if Variant = "recovery_unmaps" then
        unmap();
      end if;
      goto Teardown;
    end either;
  Remap:
    unmap();
    poolsKilled := FALSE;
    recovering := FALSE;
  Serve:
    \* With no shutdown asked for, the daemon serves on; the model stops.
    either
      await shutdown;
    or
      await ~shutdown /\ pc["signal"] = "Done" /\ \A w \in Workers : pc[w] /= "Pick";
      servingOn := TRUE;
      goto Done;
    end either;
  Grace:
    if Variant = "join_first" then
      await joined = Workers;
    else
      either
        await joined = Workers;
      or
        skip;
      end either;
    end if;
  Kill:
    if joined /= Workers then
      stop_pools();
    end if;
  Join:
    either
      await joined = Workers;
    or
      gaveUp := joined /= Workers;
    end either;
  Unmap:
    if (~gaveUp /\ Variant = "unmap_at_exit") \/ (gaveUp /\ Variant = "unmap_on_give_up") then
      exit_unmap();
    end if;
  Teardown:
    stop_pools();
  Leave:
    exited := TRUE;
    failed := panicked;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "b8371e6e" /\ chksum(tla) = "6a5df1e9")
VARIABLES poolWedged, otherEnds, state, child, joined, poolsKilled, 
          recovering, unmapUnderAWorker, background, exited, shutdown, 
          panicked, failed, servingOn, pc

(* define statement *)
NoUnmapUnderARunningWorker == ~unmapUnderAWorker
NoChildOutlivesTheDaemon == exited => \A w \in Workers : ~child[w]
ShutdownFinishes == shutdown ~> exited
APanicEndsTheDaemonAsFailed == panicked ~> (exited /\ failed)

VARIABLE gaveUp

vars == << poolWedged, otherEnds, state, child, joined, poolsKilled, 
           recovering, unmapUnderAWorker, background, exited, shutdown, 
           panicked, failed, servingOn, pc, gaveUp >>

ProcSet == (Workers) \cup {"signal"} \cup {"main"}

Init == (* Global variables *)
        /\ poolWedged \in BOOLEAN
        /\ otherEnds \in BOOLEAN
        /\ state = [w \in Workers |-> Idle]
        /\ child = [w \in Workers |-> FALSE]
        /\ joined = {}
        /\ poolsKilled = FALSE
        /\ recovering = FALSE
        /\ unmapUnderAWorker = FALSE
        /\ background \in BOOLEAN
        /\ exited = FALSE
        /\ shutdown = FALSE
        /\ panicked = FALSE
        /\ failed = FALSE
        /\ servingOn = FALSE
        (* Process Main *)
        /\ gaveUp = FALSE
        /\ pc = [self \in ProcSet |-> CASE self \in Workers -> "Pick"
                                        [] self = "signal" -> "Request"
                                        [] self = "main" -> "Recover"]

Pick(self) == /\ pc[self] = "Pick"
              /\ IF ~recovering /\ ~shutdown
                    THEN /\ \/ /\ state' = [state EXCEPT ![self] = PoolCall]
                               /\ UNCHANGED <<child, shutdown, panicked>>
                            \/ /\ state' = [state EXCEPT ![self] = OtherWait]
                               /\ UNCHANGED <<child, shutdown, panicked>>
                            \/ /\ state' = [state EXCEPT ![self] = ChildWait]
                               /\ child' = [child EXCEPT ![self] = TRUE]
                               /\ UNCHANGED <<shutdown, panicked>>
                            \/ /\ panicked' = TRUE
                               /\ IF Variant /= "panic_continues"
                                     THEN /\ shutdown' = TRUE
                                     ELSE /\ TRUE
                                          /\ UNCHANGED shutdown
                               /\ UNCHANGED <<state, child>>
                            \/ /\ TRUE
                               /\ UNCHANGED <<state, child, shutdown, panicked>>
                    ELSE /\ TRUE
                         /\ UNCHANGED << state, child, shutdown, panicked >>
              /\ pc' = [pc EXCEPT ![self] = "Wait"]
              /\ UNCHANGED << poolWedged, otherEnds, joined, poolsKilled, 
                              recovering, unmapUnderAWorker, background, 
                              exited, failed, servingOn, gaveUp >>

Wait(self) == /\ pc[self] = "Wait"
              /\ IF state[self] = PoolCall
                    THEN /\ exited \/ servingOn \/ ~poolWedged \/ poolsKilled
                    ELSE /\ IF state[self] = OtherWait
                               THEN /\ exited \/ servingOn \/ otherEnds
                               ELSE /\ IF state[self] = ChildWait
                                          THEN /\ exited \/ servingOn \/ otherEnds \/ ~child[self]
                                          ELSE /\ TRUE
              /\ IF exited \/ servingOn
                    THEN /\ pc' = [pc EXCEPT ![self] = "Done"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "Return"]
              /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                              poolsKilled, recovering, unmapUnderAWorker, 
                              background, exited, shutdown, panicked, failed, 
                              servingOn, gaveUp >>

Return(self) == /\ pc[self] = "Return"
                /\ state' = [state EXCEPT ![self] = Idle]
                /\ child' = [child EXCEPT ![self] = FALSE]
                /\ pc' = [pc EXCEPT ![self] = "Finish"]
                /\ UNCHANGED << poolWedged, otherEnds, joined, poolsKilled, 
                                recovering, unmapUnderAWorker, background, 
                                exited, shutdown, panicked, failed, servingOn, 
                                gaveUp >>

Finish(self) == /\ pc[self] = "Finish"
                /\ joined' = (joined \union {self})
                /\ pc' = [pc EXCEPT ![self] = "Done"]
                /\ UNCHANGED << poolWedged, otherEnds, state, child, 
                                poolsKilled, recovering, unmapUnderAWorker, 
                                background, exited, shutdown, panicked, failed, 
                                servingOn, gaveUp >>

Worker(self) == Pick(self) \/ Wait(self) \/ Return(self) \/ Finish(self)

Request == /\ pc["signal"] = "Request"
           /\ \/ /\ shutdown' = TRUE
              \/ /\ TRUE
                 /\ UNCHANGED shutdown
           /\ pc' = [pc EXCEPT !["signal"] = "Done"]
           /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                           poolsKilled, recovering, unmapUnderAWorker, 
                           background, exited, panicked, failed, servingOn, 
                           gaveUp >>

Signal == Request

Recover == /\ pc["main"] = "Recover"
           /\ \/ /\ pc' = [pc EXCEPT !["main"] = "Serve"]
                 /\ UNCHANGED <<poolsKilled, recovering>>
              \/ /\ poolsKilled' = TRUE
                 /\ recovering' = TRUE
                 /\ pc' = [pc EXCEPT !["main"] = "Drain"]
           /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                           unmapUnderAWorker, background, exited, shutdown, 
                           panicked, failed, servingOn, gaveUp >>

Drain == /\ pc["main"] = "Drain"
         /\ \/ /\ \A w \in Workers : state[w] = Idle
               /\ IF shutdown
                     THEN /\ recovering' = FALSE
                          /\ pc' = [pc EXCEPT !["main"] = "Serve"]
                     ELSE /\ pc' = [pc EXCEPT !["main"] = "Remap"]
                          /\ UNCHANGED recovering
               /\ UNCHANGED unmapUnderAWorker
            \/ /\ IF Variant = "recovery_unmaps"
                     THEN /\ IF \E w \in Workers : state[w] /= Idle
                                THEN /\ unmapUnderAWorker' = TRUE
                                ELSE /\ TRUE
                                     /\ UNCHANGED unmapUnderAWorker
                     ELSE /\ TRUE
                          /\ UNCHANGED unmapUnderAWorker
               /\ pc' = [pc EXCEPT !["main"] = "Teardown"]
               /\ UNCHANGED recovering
         /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                         poolsKilled, background, exited, shutdown, panicked, 
                         failed, servingOn, gaveUp >>

Remap == /\ pc["main"] = "Remap"
         /\ IF \E w \in Workers : state[w] /= Idle
               THEN /\ unmapUnderAWorker' = TRUE
               ELSE /\ TRUE
                    /\ UNCHANGED unmapUnderAWorker
         /\ poolsKilled' = FALSE
         /\ recovering' = FALSE
         /\ pc' = [pc EXCEPT !["main"] = "Serve"]
         /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                         background, exited, shutdown, panicked, failed, 
                         servingOn, gaveUp >>

Serve == /\ pc["main"] = "Serve"
         /\ \/ /\ shutdown
               /\ pc' = [pc EXCEPT !["main"] = "Grace"]
               /\ UNCHANGED servingOn
            \/ /\ ~shutdown /\ pc["signal"] = "Done" /\ \A w \in Workers : pc[w] /= "Pick"
               /\ servingOn' = TRUE
               /\ pc' = [pc EXCEPT !["main"] = "Done"]
         /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                         poolsKilled, recovering, unmapUnderAWorker, 
                         background, exited, shutdown, panicked, failed, 
                         gaveUp >>

Grace == /\ pc["main"] = "Grace"
         /\ IF Variant = "join_first"
               THEN /\ joined = Workers
               ELSE /\ \/ /\ joined = Workers
                       \/ /\ TRUE
         /\ pc' = [pc EXCEPT !["main"] = "Kill"]
         /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                         poolsKilled, recovering, unmapUnderAWorker, 
                         background, exited, shutdown, panicked, failed, 
                         servingOn, gaveUp >>

Kill == /\ pc["main"] = "Kill"
        /\ IF joined /= Workers
              THEN /\ poolsKilled' = TRUE
                   /\ IF Variant /= "children_survive"
                         THEN /\ child' = [w \in Workers |-> FALSE]
                         ELSE /\ TRUE
                              /\ child' = child
              ELSE /\ TRUE
                   /\ UNCHANGED << child, poolsKilled >>
        /\ pc' = [pc EXCEPT !["main"] = "Join"]
        /\ UNCHANGED << poolWedged, otherEnds, state, joined, recovering, 
                        unmapUnderAWorker, background, exited, shutdown, 
                        panicked, failed, servingOn, gaveUp >>

Join == /\ pc["main"] = "Join"
        /\ \/ /\ joined = Workers
              /\ UNCHANGED gaveUp
           \/ /\ gaveUp' = (joined /= Workers)
        /\ pc' = [pc EXCEPT !["main"] = "Unmap"]
        /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                        poolsKilled, recovering, unmapUnderAWorker, background, 
                        exited, shutdown, panicked, failed, servingOn >>

Unmap == /\ pc["main"] = "Unmap"
         /\ IF (~gaveUp /\ Variant = "unmap_at_exit") \/ (gaveUp /\ Variant = "unmap_on_give_up")
               THEN /\ IF background \/ \E w \in Workers : state[w] /= Idle
                          THEN /\ unmapUnderAWorker' = TRUE
                          ELSE /\ TRUE
                               /\ UNCHANGED unmapUnderAWorker
               ELSE /\ TRUE
                    /\ UNCHANGED unmapUnderAWorker
         /\ pc' = [pc EXCEPT !["main"] = "Teardown"]
         /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                         poolsKilled, recovering, background, exited, shutdown, 
                         panicked, failed, servingOn, gaveUp >>

Teardown == /\ pc["main"] = "Teardown"
            /\ poolsKilled' = TRUE
            /\ IF Variant /= "children_survive"
                  THEN /\ child' = [w \in Workers |-> FALSE]
                  ELSE /\ TRUE
                       /\ child' = child
            /\ pc' = [pc EXCEPT !["main"] = "Leave"]
            /\ UNCHANGED << poolWedged, otherEnds, state, joined, recovering, 
                            unmapUnderAWorker, background, exited, shutdown, 
                            panicked, failed, servingOn, gaveUp >>

Leave == /\ pc["main"] = "Leave"
         /\ exited' = TRUE
         /\ failed' = panicked
         /\ pc' = [pc EXCEPT !["main"] = "Done"]
         /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                         poolsKilled, recovering, unmapUnderAWorker, 
                         background, shutdown, panicked, servingOn, gaveUp >>

Main == Recover \/ Drain \/ Remap \/ Serve \/ Grace \/ Kill \/ Join
           \/ Unmap \/ Teardown \/ Leave

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Signal \/ Main
           \/ (\E self \in Workers: Worker(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in Workers : WF_vars(Worker(self))
        /\ WF_vars(Signal)
        /\ WF_vars(Main)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
