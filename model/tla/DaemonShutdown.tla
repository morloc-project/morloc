---------------------------- MODULE DaemonShutdown ----------------------------
(* Daemon shutdown (model/daemon.md DAEMON-6). A worker may be inside a     *)
(* call to a pool, which a healthy pool answers and a wedged one answers    *)
(* only once killed, inside a wait that stopping the pools does not end (a *)
(* client, an evaluation in the daemon itself), or waiting on a child      *)
(* process group it started (a forked compiler), which ends when the group *)
(* is killed. A shutdown signal may arrive at any time, including during a *)
(* pool crash recovery, which runs on the main thread. Shutdown waits a    *)
(* grace period, stops the pools and every child group, waits again, and   *)
(* then either unmaps shared memory and exits, when every worker has       *)
(* returned, or exits without unmapping, which ends the remaining workers  *)
(* with the process. No child group outlives the daemon.                   *)
EXTENDS Naturals

CONSTANT Variant
\* "bounded": the design.
\* "join_first": join the workers with no bound before stopping the pools.
\* "no_kill": after the grace period, unmap and exit without stopping pools.
\* "unmap_on_give_up": after the second wait, unmap even if workers remain.
\* "recovery_unmaps": a recovery whose wait for requests runs out unmaps.
\* "children_survive": stopping the pools leaves child groups running.

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
    exited = FALSE,
    shutdown = FALSE;

define
    NoUnmapUnderARunningWorker == ~unmapUnderAWorker
    NoChildOutlivesTheDaemon == exited => \A w \in Workers : ~child[w]
    ShutdownFinishes == shutdown ~> exited
end define;

macro unmap() begin
  if \E w \in Workers : state[w] /= Idle then
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
        skip;
      end either;
    end if;
  Wait:
    if state[self] = PoolCall then
      await exited \/ ~poolWedged \/ poolsKilled;
    elsif state[self] = OtherWait then
      await exited \/ otherEnds;
    elsif state[self] = ChildWait then
      await exited \/ otherEnds \/ ~child[self];
    end if;
    if exited then
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
    shutdown := TRUE;
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
    await shutdown;
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
    if joined /= Workers /\ Variant /= "no_kill" then
      stop_pools();
    end if;
  Join:
    if Variant /= "no_kill" then
      either
        await joined = Workers;
      or
        gaveUp := joined /= Workers;
      end either;
    end if;
  Unmap:
    if ~gaveUp \/ Variant = "unmap_on_give_up" then
      unmap();
    end if;
  Teardown:
    stop_pools();
  Leave:
    exited := TRUE;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "5958f08d" /\ chksum(tla) = "79071b0")
VARIABLES poolWedged, otherEnds, state, child, joined, poolsKilled, 
          recovering, unmapUnderAWorker, exited, shutdown, pc

(* define statement *)
NoUnmapUnderARunningWorker == ~unmapUnderAWorker
NoChildOutlivesTheDaemon == exited => \A w \in Workers : ~child[w]
ShutdownFinishes == shutdown ~> exited

VARIABLE gaveUp

vars == << poolWedged, otherEnds, state, child, joined, poolsKilled, 
           recovering, unmapUnderAWorker, exited, shutdown, pc, gaveUp >>

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
        /\ exited = FALSE
        /\ shutdown = FALSE
        (* Process Main *)
        /\ gaveUp = FALSE
        /\ pc = [self \in ProcSet |-> CASE self \in Workers -> "Pick"
                                        [] self = "signal" -> "Request"
                                        [] self = "main" -> "Recover"]

Pick(self) == /\ pc[self] = "Pick"
              /\ IF ~recovering /\ ~shutdown
                    THEN /\ \/ /\ state' = [state EXCEPT ![self] = PoolCall]
                               /\ child' = child
                            \/ /\ state' = [state EXCEPT ![self] = OtherWait]
                               /\ child' = child
                            \/ /\ state' = [state EXCEPT ![self] = ChildWait]
                               /\ child' = [child EXCEPT ![self] = TRUE]
                            \/ /\ TRUE
                               /\ UNCHANGED <<state, child>>
                    ELSE /\ TRUE
                         /\ UNCHANGED << state, child >>
              /\ pc' = [pc EXCEPT ![self] = "Wait"]
              /\ UNCHANGED << poolWedged, otherEnds, joined, poolsKilled, 
                              recovering, unmapUnderAWorker, exited, shutdown, 
                              gaveUp >>

Wait(self) == /\ pc[self] = "Wait"
              /\ IF state[self] = PoolCall
                    THEN /\ exited \/ ~poolWedged \/ poolsKilled
                    ELSE /\ IF state[self] = OtherWait
                               THEN /\ exited \/ otherEnds
                               ELSE /\ IF state[self] = ChildWait
                                          THEN /\ exited \/ otherEnds \/ ~child[self]
                                          ELSE /\ TRUE
              /\ IF exited
                    THEN /\ pc' = [pc EXCEPT ![self] = "Done"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "Return"]
              /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                              poolsKilled, recovering, unmapUnderAWorker, 
                              exited, shutdown, gaveUp >>

Return(self) == /\ pc[self] = "Return"
                /\ state' = [state EXCEPT ![self] = Idle]
                /\ child' = [child EXCEPT ![self] = FALSE]
                /\ pc' = [pc EXCEPT ![self] = "Finish"]
                /\ UNCHANGED << poolWedged, otherEnds, joined, poolsKilled, 
                                recovering, unmapUnderAWorker, exited, 
                                shutdown, gaveUp >>

Finish(self) == /\ pc[self] = "Finish"
                /\ joined' = (joined \union {self})
                /\ pc' = [pc EXCEPT ![self] = "Done"]
                /\ UNCHANGED << poolWedged, otherEnds, state, child, 
                                poolsKilled, recovering, unmapUnderAWorker, 
                                exited, shutdown, gaveUp >>

Worker(self) == Pick(self) \/ Wait(self) \/ Return(self) \/ Finish(self)

Request == /\ pc["signal"] = "Request"
           /\ shutdown' = TRUE
           /\ pc' = [pc EXCEPT !["signal"] = "Done"]
           /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                           poolsKilled, recovering, unmapUnderAWorker, exited, 
                           gaveUp >>

Signal == Request

Recover == /\ pc["main"] = "Recover"
           /\ \/ /\ pc' = [pc EXCEPT !["main"] = "Serve"]
                 /\ UNCHANGED <<poolsKilled, recovering>>
              \/ /\ poolsKilled' = TRUE
                 /\ recovering' = TRUE
                 /\ pc' = [pc EXCEPT !["main"] = "Drain"]
           /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                           unmapUnderAWorker, exited, shutdown, gaveUp >>

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
                         poolsKilled, exited, shutdown, gaveUp >>

Remap == /\ pc["main"] = "Remap"
         /\ IF \E w \in Workers : state[w] /= Idle
               THEN /\ unmapUnderAWorker' = TRUE
               ELSE /\ TRUE
                    /\ UNCHANGED unmapUnderAWorker
         /\ poolsKilled' = FALSE
         /\ recovering' = FALSE
         /\ pc' = [pc EXCEPT !["main"] = "Serve"]
         /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, exited, 
                         shutdown, gaveUp >>

Serve == /\ pc["main"] = "Serve"
         /\ shutdown
         /\ pc' = [pc EXCEPT !["main"] = "Grace"]
         /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                         poolsKilled, recovering, unmapUnderAWorker, exited, 
                         shutdown, gaveUp >>

Grace == /\ pc["main"] = "Grace"
         /\ IF Variant = "join_first"
               THEN /\ joined = Workers
               ELSE /\ \/ /\ joined = Workers
                       \/ /\ TRUE
         /\ pc' = [pc EXCEPT !["main"] = "Kill"]
         /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                         poolsKilled, recovering, unmapUnderAWorker, exited, 
                         shutdown, gaveUp >>

Kill == /\ pc["main"] = "Kill"
        /\ IF joined /= Workers /\ Variant /= "no_kill"
              THEN /\ poolsKilled' = TRUE
                   /\ IF Variant /= "children_survive"
                         THEN /\ child' = [w \in Workers |-> FALSE]
                         ELSE /\ TRUE
                              /\ child' = child
              ELSE /\ TRUE
                   /\ UNCHANGED << child, poolsKilled >>
        /\ pc' = [pc EXCEPT !["main"] = "Join"]
        /\ UNCHANGED << poolWedged, otherEnds, state, joined, recovering, 
                        unmapUnderAWorker, exited, shutdown, gaveUp >>

Join == /\ pc["main"] = "Join"
        /\ IF Variant /= "no_kill"
              THEN /\ \/ /\ joined = Workers
                         /\ UNCHANGED gaveUp
                      \/ /\ gaveUp' = (joined /= Workers)
              ELSE /\ TRUE
                   /\ UNCHANGED gaveUp
        /\ pc' = [pc EXCEPT !["main"] = "Unmap"]
        /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                        poolsKilled, recovering, unmapUnderAWorker, exited, 
                        shutdown >>

Unmap == /\ pc["main"] = "Unmap"
         /\ IF ~gaveUp \/ Variant = "unmap_on_give_up"
               THEN /\ IF \E w \in Workers : state[w] /= Idle
                          THEN /\ unmapUnderAWorker' = TRUE
                          ELSE /\ TRUE
                               /\ UNCHANGED unmapUnderAWorker
               ELSE /\ TRUE
                    /\ UNCHANGED unmapUnderAWorker
         /\ pc' = [pc EXCEPT !["main"] = "Teardown"]
         /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                         poolsKilled, recovering, exited, shutdown, gaveUp >>

Teardown == /\ pc["main"] = "Teardown"
            /\ poolsKilled' = TRUE
            /\ IF Variant /= "children_survive"
                  THEN /\ child' = [w \in Workers |-> FALSE]
                  ELSE /\ TRUE
                       /\ child' = child
            /\ pc' = [pc EXCEPT !["main"] = "Leave"]
            /\ UNCHANGED << poolWedged, otherEnds, state, joined, recovering, 
                            unmapUnderAWorker, exited, shutdown, gaveUp >>

Leave == /\ pc["main"] = "Leave"
         /\ exited' = TRUE
         /\ pc' = [pc EXCEPT !["main"] = "Done"]
         /\ UNCHANGED << poolWedged, otherEnds, state, child, joined, 
                         poolsKilled, recovering, unmapUnderAWorker, shutdown, 
                         gaveUp >>

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
